namespace TypeProviderUser.UserTypes

open Rezoom.SQL.Annotations

type UserId = UserId of int64

module Extensions =
    type System.TimeOnly with
        member this.ToPrimitive() =
            this.ToString("o")
        static member FromPrimitive(s : string) =
            System.TimeOnly.ParseExact(s, "o")

// --- Fixtures for the Rezoom.SQL.Annotations attribute pipeline ----------

/// Single-case DU with a type-level RawBackendSQLType attribute. The
/// loader's findSingleCaseDU path should pick this up and emit the
/// literal "MEDIUMINT" wherever this primitive is used in SQL.
[<RawBackendSQLType("MEDIUMINT")>]
type CompactInt = CompactInt of int

/// Single-case DU with a type-level SQLTypeLength attribute. The
/// loader should populate the length field; the backend then applies
/// its default string mapping (e.g. NVARCHAR / VARCHAR) parameterized
/// by 80.
[<SQLTypeLength(80)>]
type ShortName = ShortName of string

/// Single-case DU over byte[] — exercises the byte[] underlying-CLR-type
/// path end-to-end through the SQLite TPU.
type FileHash = FileHash of byte[]

/// Extension-method conversion on a BCL type the user does not own.
/// The attribute is method-level (on ToPrimitive) because we can't
/// place an attribute on System.DateTimeOffset itself. Mirrors the
/// existing TimeOnly pattern: instance ToPrimitive + static
/// FromPrimitive, both of which the loader's
/// toPrimitiveFSharpExtension / fromPrimitive regex picks up off the
/// compiled module class.
module DateTimeOffsetExtensions =
    type System.DateTimeOffset with
        [<RawBackendSQLType("DATETIMEOFFSET(7)")>]
        member this.ToPrimitive() = this.ToString("o")
        static member FromPrimitive(s : string) =
            System.DateTimeOffset.ParseExact(
                s, "o", System.Globalization.CultureInfo.InvariantCulture)

// Interfaces exposed for SELECT<...> row-type implementation tests
// (TypeProviderUser.SQLite.TestRowTypeInterfaces).

type IUserSummary =
    abstract member Name : string
    abstract member Email : string

type IHasUserId =
    abstract member Id : UserId

type IHasBedtime =
    abstract member Name : string
    abstract member BedtimeIfAny : System.TimeOnly option

type IArticleHeader =
    abstract member ArticleTitle : string
    abstract member ArticleText : string

type IUserWithArticles =
    abstract member Name : string
    abstract member Articles : System.Collections.Generic.IReadOnlyList<IArticleHeader>

// Same shape as IUserWithArticles but the nav declares the broader
// IEnumerable<T> supertype — exercises the covariant cross-interface
// upcast (row has IReadOnlyList<ArticleRow>, interface wants IEnumerable<IArticleHeader>).
type IUserWithArticlesEnumerable =
    abstract member Name : string
    abstract member Articles : System.Collections.Generic.IEnumerable<IArticleHeader>

type IArticleWithAuthor =
    abstract member ArticleTitle : string
    abstract member Author : IUserSummary

type IPictureRef =
    abstract member SHA256 : byte array

type IUserWithMaybePicture =
    abstract member Name : string
    abstract member Picture : IPictureRef option