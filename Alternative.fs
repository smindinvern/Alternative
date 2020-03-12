namespace smindinvern

module Alternative =

    module Errors =
        type Tree<'a> =
            { Value: 'a;
              Children: Tree<'a> list } with
            override x.ToString() = sprintf "{ %A: %A }" x.Value x.Children
            

    open State.Lazy
    /// <typeparam name="'u">The type of the extra state available to the monad.</typeparam>
    /// <typeparam name="'e">The type of error produced by the monad.</typeparam>
    /// <typeparam name="'a">The type of the value produced by the monad.</typeparam> 
    type Alternative<'u, 'e, 'a> = State<'u * bool, Result<'a, Errors.Tree<'e>>>

    /// <summary>
    /// Models short-circuit logical-OR on Alternatives.
    ///
    /// (f <|> g) produces a value IFF one of the two Alternatives succeeds.
    /// If both Alternatives fail, their errors are concatenated.
    /// </summary>
    /// <param name="f">The first Alternative to try.</param>
    /// <param name="g">The second Alternative to try.</param>
    let (<|>) (f: Alternative<'u, 'e, 'a>) (g: Alternative<'u, 'e, 'a>) : Alternative<'u, 'e, 'a> =
        // Implement choice with possibility of failure
        State.Lazy.state {
            let! s = get
            match! f with
            | Result.Error e1 ->
                let! (_, abort) = get
                if abort then
                    return Result.Error e1
                else
                    // Back-track to previous state.
                    let! _ = put s
                    match! g with
                    | Result.Error e2 -> return Result.Error { e1 with Children = e1.Children @ [ e2 ] }
                    | x -> return x
            | x -> return x
        }

    /// <summary>
    /// Right-associative version of <see cref="(<|>)" />.
    /// </summary>
    let inline ( ^|^ ) f g = f (<|>) g
    
    /// <summary>
    /// Encapsulate a Result value within an Alternative.  The resulting
    /// Alternative will either produce a value, or an error, according to
    /// the value of the given Result.
    /// </summary>
    /// <param name="r">The Result value to inject.</param>
    let inline liftResult (r: Result<'a, Errors.Tree<'e>>) : Alternative<'u, 'e, 'a> =
        inject r

    /// <summary>
    /// Produce an error.
    ///
    /// Computation may still continue via back-tracking.
    /// </summary>
    /// <param name="message">The error message.</param>
    let inline error (e: 'e) : Alternative<'u, 'e, 'a> =
        liftResult <| Result.Error { Errors.Tree.Value = e; Errors.Tree.Children = [] }
        
    module Monad =
        open Utils
        
        let inline inject (v: 'a) : Alternative<'u, 'e, 'a> =
            state {
                return Result.Ok v
            }
        let inline bind (f: 'a -> Alternative<'u, 'e, 'b>) (c: Alternative<'u, 'e, 'a>) =
            state {
                match! c with
                | Result.Error e -> return Result.Error e
                | Result.Ok a -> return! f a
            }

        let inline (>>=) m f = bind f m

        type Alternative() =
            member inline __.Bind(m, f) = m >>= f
            member inline __.Return(v) = inject v
            member inline __.ReturnFrom(m) = m
            member inline __.Combine(a, b) = a >>= (konst b)

        let alt = Alternative()

        /// <summary>
        /// Function application lifted into the Alternative monad.
        ///
        /// Just as with normal function application, (<@>) is left-associative.
        /// </summary>
        let inline (<@>) (f: 'a -> 'b) (c: Alternative<'u, 'e, 'a>) : Alternative<'u, 'e, 'b> =
            bind (inject << f) c

        /// <summary>
        /// Sequential application of an Alternative.
        /// </summary>
        let inline (<*>) (f: Alternative<'u, 'e, 'a -> 'b>) (c: Alternative<'u, 'e, 'a>) : Alternative<'u, 'e, 'b> =
            bind (fun f' -> f' <@> c) f
        
        let sequence (cs: Alternative<'u, 'e, 'a> list) : Alternative<'u, 'e, 'a list> =
            List.foldBack (fun t s -> List.cons <@> t <*> s) cs (inject [])

    /// <summary>
    /// Attempt to evaluate and return a value.  If the evaluation throws an exception, produce
    /// an error containing the Message property of the exception.
    /// </summary>
    /// <param name="v">The value to evaluate.</param>
    let inline catch (v: Lazy<'a>) =
        try
            Monad.inject <| v.Force()
        with
            | x -> error <| x.Message

    open smindinvern.Utils

    let inline get<'u, 'e> : Alternative<'u, 'e, 'u> =
        get >>= (Monad.inject << fst)
    let inline modify (f: 'u -> 'u) : Alternative<'u, 'e, unit> =
        (modify (fun (u, b) -> (f u, b))) >>= Monad.inject
    let inline put (u: 'u) : Alternative<'u, 'e, unit> =
        modify (konst u)

    /// <summary>
    /// Terminate computation immediately, producing an error.
    /// </summary>
    /// <param name="msg">The error message.</param>
    let abort (e: 'e) : Alternative<'u, 'e, 'a> =
        state {
            do! State.Lazy.modify (fun (u, _) -> (u, true))
            return! error e
        }
