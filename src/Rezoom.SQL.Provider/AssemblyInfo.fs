namespace Rezoom.SQL.Provider.AssemblyInfo

open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

// General Information about an assembly is controlled through the following 
// set of attributes. Change these attribute values to modify the information
// associated with an assembly.
[<assembly: AssemblyTitle("Rezoom.SQL.Provider")>]
[<assembly: AssemblyDescription("Implements an F# type provider for SQL statements.")>]
[<assembly: AssemblyConfiguration("")>]
[<assembly: AssemblyCompany("Robert Peele")>]
[<assembly: AssemblyProduct("Rezoom.SQL")>]
[<assembly: AssemblyCopyright("Copyright © Robert Peele 2016")>]
[<assembly: AssemblyTrademark("")>]
[<assembly: AssemblyCulture("")>]

// Setting ComVisible to false makes the types in this assembly not visible 
// to COM components.  If you need to access a type in this assembly from 
// COM, set the ComVisible attribute to true on that type.
[<assembly: ComVisible(false)>]

// The following GUID is for the ID of the typelib if this project is exposed to COM
[<assembly: Guid("7b1765cb-23f8-419a-9cc6-3da319ed066f")>]

// Version information for an assembly consists of the following four values:
// 
//       Major Version
//       Minor Version 
//       Build Number
//       Revision
// 
// You can specify all the values or you can default the Build and Revision Numbers 
// by using the '*' as shown below:
// [<assembly: AssemblyVersion("1.0.*")>]
[<assembly: AssemblyVersion("1.0.0.0")>]
[<assembly: AssemblyFileVersion("1.0.0.0")>]

#if !DEBUG
[<assembly: AssemblyKeyFile("../../../robert.peele.snk")>]
#endif

do
    ()