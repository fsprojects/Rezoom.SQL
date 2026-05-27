namespace TypeProviderUser.UserTypes

type UserId = UserId of int64

module Extensions =
    type System.TimeOnly with
        member this.ToPrimitive() =
            this.ToString("o")
        static member FromPrimitive(s : string) =
            System.TimeOnly.ParseExact(s, "o")