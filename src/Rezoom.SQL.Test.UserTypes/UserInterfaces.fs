namespace Rezoom.SQL.Test.UserTypes

type IFoo =
    interface end

type IBar =
    interface end

type IRowXYZ =
    abstract member X : int
    abstract member Y : int
    abstract member Z : int