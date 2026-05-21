namespace Rezoom.SQL.Test.UserTypes

// Sample user primitives that we can test materializing from Rezoom.SQL.Mapping!
// The implementations here are trashy placeholders with no error checking, no validation!
// We just care about testing the mapping calls the right methods.

/// A type that declares its own ToPrimitive and FromPrimitive.
type StringyPhoneNumber =
    {   CountryCode : int
        AreaCode : int
        Number : int
    }
    static member ToPrimitive(phone : StringyPhoneNumber) =
        sprintf "+%1d%03d%07d" phone.CountryCode phone.AreaCode phone.Number
    static member FromPrimitive(str : string) = 
        {   CountryCode = int (str.[1..2])
            AreaCode = int (str.[3..5])
            Number = int (str.[6..])
        }

/// ToPrimitive and FromPrimitive will be declared as extensions in the consumer test assembly.
/// This should also work.
type IntyPhoneNumber(countryCode : int, areaCode : int, num : int) =
    member this.CountryCode = countryCode
    member this.AreaCode = areaCode
    member this.Number = num

/// Should fail because it has two FromPrimitive definitions.
type TooManyConverters() =
    static member ToPrimitive(x : TooManyConverters) = 0
    static member FromPrimitive(x : int) = TooManyConverters()
    static member FromPrimitive(x : string) = TooManyConverters()

/// Testing that we can add primitive converters for types beyond our control,
/// and could even override the default Rezoom.SQL.Mapping handling to remap a primitive
/// it supports natively like DateTimeOffset and represent it as a string instead.
module ExtendingSystemPrimitives =
    // F#-style extension methods can be used.
    type System.DateTimeOffset with
        member this.ToPrimitive() = this.ToString("o")
        static member FromPrimitive(x : string) = System.DateTimeOffset.TryParse(x)
    
    // F# let-bound functiosn can be used, as long as they are PascalCase.
    let ToPrimitive (ts : System.TimeSpan) = ts.Ticks
    let FromPrimitive (ticks : int64) = System.TimeSpan.FromTicks(ticks)

// Static methods found any-which-where can be used
type AdhocExtensionClass() =
    static member ToPrimitive(t : System.TimeOnly) = t.ToString("o")
    static member FromPrimitive(str : string) = System.TimeOnly.ParseExact(str, "o")