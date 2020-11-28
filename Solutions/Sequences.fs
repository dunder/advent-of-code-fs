module AoC.Sequences

let count x = Seq.filter ((=) x) >> Seq.length