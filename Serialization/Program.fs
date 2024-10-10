open System
open System.Collections.Generic

module Result =
    type ResultBuilder() =
        member this.Return(x) = Ok x
        member this.Bind(x, f) = Result.bind f x

    let result = ResultBuilder()

module Domain =
    type String50 = String50 of string

    module String50 =
        let create fieldName str : Result<String50, string> =
            if String.IsNullOrEmpty str then
                Error $"{fieldName} cannot be empty"
            elif str.Length > 50 then
                Error $"{fieldName} must be less than 50 characters"
            else
                Ok(String50 str)

        let value (String50 s) = s

    type Birthdate = Birthdate of DateTime

    module Birthdate =
        let create (dt: DateTime) : Result<Birthdate, string> =
            if dt > DateTime.Now then
                Error "Birthdate cannot be in the future"
            else
                Ok(Birthdate dt)

        let value (Birthdate dt) = dt

    type Person =
        { First: String50
          Last: String50
          Birthdate: Birthdate }

open Domain

module Dto =
    type Person =
        { First: string
          Last: string
          Birthdate: DateTime }

    module Person =
        let fromDomain (person: Domain.Person) : Person =
            let first = person.First |> String50.value
            let last = person.Last |> String50.value
            let birthdate = person.Birthdate |> Birthdate.value

            { First = first
              Last = last
              Birthdate = birthdate }

        let toDomain (dto: Person) : Result<Domain.Person, string> =
            Result.result {
                let! first = dto.First |> String50.create "First"
                let! last = dto.Last |> String50.create "Last"
                let! birthdate = dto.Birthdate |> Birthdate.create

                return
                    { First = first
                      Last = last
                      Birthdate = birthdate }
            }

module Json =
    open System.Text.Json
    let serialize obj = JsonSerializer.Serialize(obj)

    let deserialize<'T> (jsonStr: string) =
        try
            JsonSerializer.Deserialize<'T> jsonStr |> Ok
        with ex ->
            Error ex

let jsonFromDomain (person: Domain.Person) =
    person |> Dto.Person.fromDomain |> Json.serialize

type DtoError =
    | ValidationError of string
    | DeserializationException of exn

let jsonToDomain jsonString : Result<Domain.Person, DtoError> =
    Result.result {
        let! deserializedValue = jsonString |> Json.deserialize |> Result.mapError DeserializationException
        let! domainValue = deserializedValue |> Dto.Person.toDomain |> Result.mapError ValidationError
        return domainValue
    }

let domain: Domain.Person =
    let first = String50 "John"
    let last = String50 "Doe"
    let birthdate: Birthdate = Birthdate(DateTime.Now.AddYears(-30))

    { First = first
      Last = last
      Birthdate = birthdate }


let jsonStr = jsonFromDomain domain
printfn $"%A{jsonStr}"

let jsonPerson =
    """{
    "First": "John",
    "Last": "Doe",
    "Birthdate": "1991-09-21T00:03:12"
}"""

jsonToDomain jsonPerson |> printfn "%A"

let jsonPersonWithErrors =
    """{
    "First": "",
    "Last": "Doe",
    "Birthdate": "1991-09-21T00:03:12"
}"""

jsonToDomain jsonPersonWithErrors |> printfn "%A"

type Name = { First: String50; Last: String50 }

type Example =
    | A
    | B of int
    | C of string list
    | D of Name

type NameDto = { First: string; Last: string }

type ExampleDto =
    { Tag: string
      BData: Nullable<int>
      CData: string[]
      DData: NameDto }

let nameDtoFromDomain (name: Name) : NameDto =
    let first = name.First |> String50.value
    let last = name.Last |> String50.value
    { First = first; Last = last }

let fromDomain (domainObj: Example) : ExampleDto =
    let nullBData = Nullable()
    let nullCData = null
    let nullDData = Unchecked.defaultof<NameDto>

    match domainObj with
    | A ->
        { Tag = "A"
          BData = nullBData
          CData = nullCData
          DData = nullDData }
    | B number ->
        { Tag = "A"
          BData = Nullable number
          CData = nullCData
          DData = nullDData }
    | C strList ->
        { Tag = "A"
          BData = nullBData
          CData = strList |> List.toArray
          DData = nullDData }
    | D name ->
        { Tag = "A"
          BData = nullBData
          CData = nullCData
          DData = name |> nameDtoFromDomain }

let nameDtoToDomain (nameDto: NameDto): Result<Name, string> =
    Result.result {
        let! first = nameDto.First |> String50.create "First"
        let! last = nameDto.Last |> String50.create "Last"
        return { First = first; Last = last }
    }

let toDomain dto: Result<Example, string> =
    match dto.Tag with
    | "A" -> Ok A
    | "B" ->
        if dto.BData.HasValue then
            dto.BData.Value |> B |> Ok
        else
            Error "B data not expected to be null"
    | "C" ->
        match dto.CData with
        | null -> Error "C data not expected to be null"
        | _ ->
            dto.CData |> Array.toList |> C |> Ok
    | "D" ->
        match box dto.DData with
        | null -> Error "D data not expected to be null"
        | _ ->
            dto.DData |> nameDtoToDomain |> Result.map D
    | _ ->
        let msg = $"Tag '{dto.Tag}' is not a valid tag"
        Error msg
        
let nameDtoMapFromDomain (name: Name): IDictionary<string, obj> =
    let first = name.First |> String50.value :> obj
    let last = name.Last |> String50.value :> obj
    [
        ("First", first)
        ("Last", last)
    ] |> dict

let dictFromDomain (domainObj: Example): IDictionary<string, obj> =
    match domainObj with
    | A ->
        [("A", null)] |> dict
    | B number ->
        [("B", Nullable number :> obj)] |> dict
    | C strList ->
        [("C", strList |> List.toArray :> obj)] |> dict
    | D name ->
        [("D", name |> nameDtoMapFromDomain :> obj)] |> dict

let getValue key (dict: IDictionary<string, obj>): Result<'a, string> =
    match dict.TryGetValue key with
    | true, value ->
        try
            // downcast to the type `a
            value :?> 'a |> Ok
        with
        | :? InvalidCastException ->
            let typeName = typeof<'a>.Name
            let msg = $"Value for key '{key}' is not of type '{typeName}'"
            Error msg
    | false, _ ->
        let msg = sprintf $"Key '%s{key} not found"
        Error msg

let dictToDomain (nameDto: IDictionary<string, obj>): Result<Name, string> =
    Result.result {
        let! firstStr = nameDto |> getValue "First"
        let! first = firstStr |> String50.create "First"
        let! lastStr = nameDto |> getValue "Last"
        let! last = lastStr |> String50.create "Last"
        return { First = first; Last = last }
    }

let dictToExample (dto: IDictionary<string, obj>): Result<Example, string> =
    if dto.ContainsKey "A" then
        Ok A
    elif dto.ContainsKey "B" then
        Result.result {
            let! bData = dto |> getValue "B"
            return B bData
        }
    elif dto.ContainsKey "C" then
        Result.result {
            let! cData = dto |> getValue "C"
            return cData |> Array.toList |> C
        }
    elif dto.ContainsKey "D" then
        Result.result {
            let! dData = dto |> getValue "D"
            let! name = dData |> nameDtoToDomain
            return D name
        }
    else
        let msg = sprintf "No union case recognized"
        Error msg

type ResultDto<'OkData, 'ErrorData when 'OkData : null and 'ErrorData: null> = {
    IsError: bool
    OkData: 'OkData
    ErrorData: 'ErrorData
}
