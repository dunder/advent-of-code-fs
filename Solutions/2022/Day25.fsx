// --- Day 25: Full of Hot Air ---

open System.IO

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let input = File.ReadLines(@".\Input\Day25.txt") |> List.ofSeq

let example = [
    "1=-0-2"
    "12111"
    "2=0="
    "21"
    "2=01"
    "111"
    "20012"
    "112"
    "1=-1="
    "1-12"
    "12"
    "1="
    "122"
]

let snafuDigitToDecimal = function
    | '2' -> 2L
    | '1' -> 1L
    | '0' -> 0L
    | '-' -> -1L
    | '=' -> -2L
    | c -> failwithf "Unrecognized input: %c" c

let decimalToSnafuDigit = function
    | 2L -> '2'
    | 1L -> '1'
    | 0L -> '0'
    | -1L -> '-'
    | -2L -> '='
    | n -> failwithf "No SNAFU digit for: %i" n

let snafuToDecimal (number: string) =
    number 
    |> Seq.rev 
    |> Seq.mapi (fun i c -> 
        let factor = if i = 0 then 1L else pown 5 i
        factor * snafuDigitToDecimal c
    ) 
    |> Seq.sum

// successive euclidean division by 5

let findNext b (n: int64) =
    
    let rec loop b n i =
        if n / (pown b i) = 0L then
            i - 1
        else
            loop b n (i+1)

    loop b n 0

let toOtherBase b n =
    let p = findNext b n

    let rec loop b n i =
        printfn "n: %i i: %i" n i
        let t = pown b i
        let digit = n / t 
        let reminder = n % t
        printfn "digit: %i reminder: %i" digit reminder

        seq {
            yield digit
            if i > 0 then
                yield! loop b reminder (i-1)
        }
    
    loop b n p

toOtherBase 8 125 |> Seq.toList
toOtherBase 5 75 |> Seq.toList


let decmalToSnafu number =
    number

pown 5 4

let factors = [0..20] |> List.map (fun x -> pown 5L x)

let sumTo n =
    factors |> List.take n |> List.map (fun f -> f * 2L) |> List.sum

let unfoldFactors limit =
    List.unfold(fun (i, f, acc1, acc2) -> 
        if acc2 >= limit then
            None
        else
            let newF = pown 5L i
            let newAcc2 = acc2 + 2L*newF
            let newAcc1 = acc1 + newF
            let next = i+1, newF, newAcc1, newAcc2
            Some(next, next)
    ) (0, 0L, 0L, 0L)



// 1  1    =              1*1
// 2  2    =              2*1
// ================================================
// 3  5-2  =        1*5 - 2*1
// 4  5-1  =        1*5 - 1*1
// 5  5    =        1*5 + 0*1
// 
// 6 5+1   =        1*5 + 1*1
// 7 5+2   =        1*5 + 2*1
// 8 10-2  =        2*5 - 2*1
// 9 10-1  =        2*5 - 1*1
// 10 10   =        2*5 + 0*1
// 
// 11 10+1 =        2*5 + 1*1
// 12 10+2 =        2*5 + 2*1
// ================================================
// 13 15-2 = 1*25 - 2*5 - 2*1
// 14 15-1 = 1*25 - 2*5 - 1*1
// 15 15   = 1*25 - 2*5 + 0*1
// 
// 16 15+1 = 1*25 - 2*5 + 1*1
// 17 15+2 = 1*25 - 2*5 + 2*1
// 18 20-2 = 1*25 - 1*5 - 2*1
// 19 20-1 = 1*25 - 1*5 - 1*1
// 20 20   = 1*25 - 1*5 + 0*1 
//
// 21 20+1 = 1*25 - 1*5 + 1*1
// 22 20+2 = 1*25 - 1*5 + 2*1
// 23 25-2 = 1*25 + 0*5 - 2*1
// 24 25-1 = 1*25 + 0*5 - 1*1
// 25 25   = 1*25 + 0*5 + 0*1

// 26 25+1 = 1*25 + 0*5 + 1*1
// 27 25+2 = 1*25 + 0*5 + 2*1
// 28 30-2 = 1*25 + 1*5 - 2*1
// 29 30-1 = 1*25 + 1*5 - 1*1
// 30 30   = 1*25 + 1*5 + 0*1

// 31 30+1 = 1*25 + 1*5 + 1*1
// 32 30+2 = 1*25 + 1*5 + 2*1
// 33 35-2 = 1*25 + 2*5 - 2*1
// 34 35-1 = 1*25 + 2*5 - 1*1
// 35 35   = 1*25 + 2*5 + 0*1

// 36 35+1 = 1*25 + 2*5 + 1*1
// 37 35+2 = 1*25 + 2*5 + 2*1
// --------------------------
// 38 40-2 = 2*25 - 2*5 - 2*1
// 39 40-1 = 2*25 - 2*5 - 1*1
// 40 40   = 2*25 - 2*5 + 0*1


// 56      =         2*25 + 1*5 + 1*1
// 57      =         2*25 + 1*5 + 2*1
// 58      =         2*25 + 2*5 - 2*1
// 59      =         2*25 + 2*5 - 1*1
// 60      =         2*25 + 2*5 + 0*1

// 61      =         2*25 + 2*5 + 1*1
// 62      =         2*25 + 2*5 + 2*1
// ================================================
// 63      = 1*125 - 2*25 - 2*5 - 2*1 = 125 - 62
// 64      = 1*125 - 2*25 - 2*5 - 1*1
// 65      = 1*125 - 2*25 - 2*5 + 0*1

// 66      = 1*125 - 2*25 - 2*5 + 1*1
// 67      = 1*125 - 2*25 - 2*5 + 2*1
// 68      = 1*125 - 2*25 - 1*5 - 2*1
// 69      = 1*125 - 2*25 - 1*5 - 2*1
// 70      = 1*125 - 2*25 - 1*5 + 0*1

// 71      = 1*125 - 2*25 - 1*5 + 1*1
// 72      = 1*125 - 2*25 - 1*5 + 2*1
// 73      = 1*125 - 2*25 - 1*5 - 2*1
// 74      = 1*125 - 2*25 - 1*5 - 1*1
// 75      = 1*125 - 2*25 - 1*5 + 0*1

// 76      = 1*125 - 2*

let snafuDigits = [1L;2L;-2L;-1L; 0L]

// 1  1    =              1*1
// 2  2    =              2*1
// ================================================
// 3  5-2  =        1*5 - 2*1
// 4  5-1  =        1*5 - 1*1
// 5  5    =        1*5 + 0*1
// 
// 6 5+1   =        1*5 + 1*1
// 7 5+2   =        1*5 + 2*1
// 8 10-2  =        2*5 - 2*1
// 9 10-1  =        2*5 - 1*1
// 10 10   =        2*5 + 0*1
// 
// 11 10+1 =        2*5 + 1*1
// 12 10+2 =        2*5 + 2*1
// ================================================
// 13 15-2 = 1*25 - 2*5 - 2*1

let test n = snafuDigits[((n-3)/5)%5]
test 13

let calc n = 
    [
        snafuDigits[(n-1)%5]
        snafuDigits[((n-3)/5)%5]
        snafuDigits[((n-3)/25)%5]
        snafuDigits[((n-3)/125%5)]
    ]
    |> List.rev

let decimalToSnafu (n: int64) =
    printfn "decimalToSnafu %i" n
    List.unfold(fun (i, shift, digit, acc) ->
        printfn "i: %i, acc: %i" i acc
        if acc = n then
            None
        else
            let p = pown 5L (i-1)
            let index = ((n-shift)/p)%5L
            let digit = snafuDigits[index |> int]
            printfn "    p: %i, index: %i, digit %i, acc + (digit*p) = %i: %i + (%i * %i) = %i" p index digit acc digit p (digit*p) (acc+digit*p)
            let acc = acc + digit*p
            let next = i+1, shift + 2L*p, digit, acc
            Some(next, next)
    ) (1,1L, 0L,0L)
    |> List.rev
    |> List.map (fun (_, _, digit, _) -> digit)
    |> List.map decimalToSnafuDigit
    |> List.toArray
    |> System.String

let printSnafu stop =
    [1..stop]
    |> List.iter (fun n -> 
        let snafu = decimalToSnafu n
        printfn "%4i: %s" n snafu
    )
pown 5 1
printSnafu 10

"2=-1=0" |> snafuToDecimal

// 26 25+1 = 1*25 + 0*5 + 1*1
// 27 25+2 = 1*25 + 0*5 + 2*1
// 28 30-2 = 1*25 + 1*5 - 2*1
// 29 30-1 = 1*25 + 1*5 - 1*1
// 30 30   = 1*25 + 1*5 + 0*1

calc 28

decimalToSnafu 1
decimalToSnafu 2
decimalToSnafu 6
decimalToSnafu 7
decimalToSnafu 28
decimalToSnafu 38
decimalToSnafu 70
decimalToSnafu 1000
decimalToSnafu 1251

[1L..1251L] |> List.map decimalToSnafu


snafuToDecimal "20001"

calc 1
calc 2
calc 3
calc 4
calc 5
calc 6
calc 38

// max input: 14860728853219L

4890L |> decimalToSnafu

// That's not the right answer. If you're stuck, make sure you're using the full input data;
// there are also some general tips on the about page, or you can ask for hints on the subreddit.
// Please wait one minute before trying again. (You guessed 37018883152395.) [Return to Day 25]

let firstStar =
    input
    |> List.map snafuToDecimal
    |> List.sum
    |> decimalToSnafu

firstStar

let secondStar = 
    0

secondStar

