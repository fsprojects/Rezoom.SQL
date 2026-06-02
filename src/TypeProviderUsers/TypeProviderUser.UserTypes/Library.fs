namespace TypeProviderUser.UserTypes

type UserId = UserId of int64

module Extensions =
    type System.TimeOnly with
        member this.ToPrimitive() =
            this.ToString("o")
        static member FromPrimitive(s : string) =
            System.TimeOnly.ParseExact(s, "o")

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