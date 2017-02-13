module Digits

let inline private ofNumber ten n =
    let rec separate n digits =
        if n < ten then
            n :: digits
        else
            separate (n / ten) ((n % ten) :: digits)
    separate n []

let ofInt = ofNumber 10
let ofBigInt = ofNumber 10I

let toInt digits =
    Seq.fold (fun acc digit -> 10 * acc + digit) 0 digits
