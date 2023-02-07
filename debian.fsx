#r "nuget: FParsec"
#r "nuget: FsToolkit.ErrorHandling"


// namespace Debian

open System
open FsToolkit.ErrorHandling
open FParsec

module Version =

    type IntString =
        | Number of string

        member this.IntValue =
            match this with Number str -> str |> int32

        static member create(str: string) =
            match (str |> int32) with
            | i when i >= 0 -> Number str
            | _ -> failwith $"{nameof (IntString)} must be a positive integer"

        override this.ToString() =
            match this with Number str -> str

    type Epoch = Epoch of IntString

    [<CustomComparison; StructuralEquality>]
    type Segment =
        | Number of IntString
        | String of string
        | Tilde

        interface IComparable<Segment> with
            member this.CompareTo(segment: Segment) : int =
                match (this, segment) with
                | Number a, Number b -> a.IntValue.CompareTo(b.IntValue)
                | String a, String b -> a.CompareTo(b)
                | Number a, String b -> a.ToString().CompareTo(b)
                | String a, Number b -> a.CompareTo(string b)
                | Tilde, Tilde -> 0
                | Tilde, _ -> 1
                | _, Tilde -> -1

        override this.ToString() =
            match this with
            | Number i -> string i
            | String str -> str
            | Tilde -> "~"

    [<CustomComparison>]
    [<CustomEquality>]
    type SegmentList =
        { Segments: Segment list }

        static member create segments = { Segments = segments }

        static member create((a: Segment list), (b: Segment list list)) =
            let segments = a @ (b |> List.collect id)
            { Segments = segments }

        interface IComparable<SegmentList> with
            member this.CompareTo(upstreamVersion) : int =
                let rec compare (x: #IComparable<_> list) (y: #IComparable<_> list) =
                    match x, y with
                    | [], [] -> 0
                    | _, [] -> 1
                    | [], _ -> -1
                    | x :: xs, y :: ys when x = y -> compare xs ys
                    | x :: _, y :: _ -> x.CompareTo(y)

                let ({ Segments = x }, { Segments = y }) = this, upstreamVersion
                compare x y

        interface IComparable with
            member this.CompareTo segment =
                match segment with
                | :? SegmentList as y -> (this :> IComparable<_>).CompareTo y
                | _ -> invalidArg (nameof segment) "cannot compare values of different types"

        override this.Equals(segment) =
            match segment with
            | :? SegmentList as y -> (compare this y) = 0
            | _ -> false

        override this.GetHashCode() = hash this

        override this.ToString() =
            this.Segments |> List.map string |> String.concat ""

    module Parsing =

        open FsToolkit.ErrorHandling.Operator.Result

        [<CustomComparison>]
        [<CustomEquality>]
        type DebianVersion =
            { Epoch: Epoch option
              UpstreamVersion: SegmentList
              DebianRevision: SegmentList option }

            interface IComparable<DebianVersion> with
                member this.CompareTo(debianVersion) : int =
                    match (this, debianVersion) with
                    | { DebianVersion.Epoch = e1 }, { DebianVersion.Epoch = e2 } when e1 <> e2 -> compare e1 e2
                    | { DebianVersion.UpstreamVersion = v1 }, { DebianVersion.UpstreamVersion = v2 } when v1 <> v2 ->
                        compare v1 v2
                    | { DebianVersion.DebianRevision = rev1 }, { DebianVersion.DebianRevision = rev2 } ->
                        compare rev1 rev2

            interface IComparable with
                member this.CompareTo version =
                    match version with
                    | :? DebianVersion as y -> (this :> IComparable<_>).CompareTo y
                    | _ -> invalidArg (nameof version) "cannot compare values of different types"

            override this.Equals(version) =
                match version with
                | :? DebianVersion as y -> (this = y)
                | _ -> false

            override this.GetHashCode() = hash this

        type SegmentParser = Parser<Segment, unit>

        let hyphen: Parser<unit, unit> = skipChar '-'

        let versionSegmentDelimiter: Parser<unit,unit> = skipAnyOf [ '.'; '+'; ':' ] <|> hyphen

        let revisionSegmentDelimiter : Parser<unit, unit> = skipAnyOf [ '.'; '+' ]

        let pTilde: Parser<Segment,unit> = pchar '~' >>% Tilde

        let pEpoch : Parser<Epoch option, unit> =
            let intEpoch =
                attempt (manyChars digit .>> skipChar ':')
                |>> (IntString.create >> Epoch >> Some)

            let nonIntEpoch =
                attempt (skipMany1Till skipAnyChar (skipChar ':'))
                .>> fail "Epoch must be a positive integer"
                >>% None

            let noEpoch = preturn None
            choice [ intEpoch; nonIntEpoch; noEpoch ]

        let pNumericSegment : SegmentParser =
            (many1Chars digit) |>> (string >> IntString.create >> Number)

        let pString : SegmentParser = (many1Chars letter) |>> String

        let pSegment : SegmentParser = choice [ pNumericSegment; pTilde; pString ]

        let pSegments: Parser<Segment list,unit> = many pSegment

        let pDebianRevision : Parser<SegmentList option, unit> =
            let debianRevision =
                hyphen >>. pSegments .>>. (many (revisionSegmentDelimiter >>. pSegments))
                .>> eof
                |>> (SegmentList.create >> Some)

            let noDebianRevision = eof >>% None
            choice [ debianRevision; noDebianRevision ]

        let pUpstreamVersion: Parser<SegmentList,unit> =
            pSegments
            .>>. (manyTill (versionSegmentDelimiter >>. pSegments) (followedBy pDebianRevision))
            |>> SegmentList.create

        let pDebianVersion: Parser<DebianVersion,unit> =
            parse {
                let! epoch = pEpoch
                let! upstreamVersion = pUpstreamVersion
                let! debianRevision = pDebianRevision

                return
                    { Epoch = epoch
                      UpstreamVersion = upstreamVersion
                      DebianRevision = debianRevision }
            }

        let mapResult (result: ParserResult<'a,'b>) =
            match result with
            | Success(res, _, _) -> Result.Ok res
            | Failure(err, _, _) -> Result.Error err

    open Parsing

    let compare (v1: string) (v2: string) =
        result {
            let! a' = (run pDebianVersion v1) |> mapResult
            let! b' = (run pDebianVersion v2) |> mapResult

            match (a', b') with
            | (x, y) when x > y -> return v1
            | (x, y) when x < y -> return v2
            | _, _ -> return v2
        }
        |> (function
        | Result.Ok x -> x.ToString()
        | Result.Error e -> e)


Version.compare "abc-123" "xyz-000"

// run (pDebianVersion) "666:123+xyz+777-jjj-kkaa333+3"
