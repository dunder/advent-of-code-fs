module AoC.Cryptography

open System.Security.Cryptography

let md5 (input : string) =
    use md5 = MD5.Create()
    input
    |> System.Text.Encoding.ASCII.GetBytes
    |> md5.ComputeHash
    |> Seq.map (fun c -> c.ToString("X2"))
    |> Seq.reduce (+)
