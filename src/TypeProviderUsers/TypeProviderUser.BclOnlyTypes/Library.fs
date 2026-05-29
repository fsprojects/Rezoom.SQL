namespace TypeProviderUser.BclOnlyTypes

// Deliberately NO single-case DU here — only a BCL-type extension. This exercises
// the SourceAssemblies declaring-assembly path: there is no user-DLL-typed
// UserCLRType to register this assembly, so it only gets registered via the
// declaring assembly of ToPrimitive/FromPrimitive (which is this DLL).
module Extensions =
    type System.DateOnly with
        member this.ToPrimitive() =
            this.ToString("o")
        static member FromPrimitive(s : string) =
            System.DateOnly.ParseExact(s, "o")
