using System.Web;
using System.Web.Optimization;

namespace SQLFiddle.Website
{
    public class BundleConfig
    {
        // For more information on bundling, visit https://go.microsoft.com/fwlink/?LinkId=301862
        public static void RegisterBundles(BundleCollection bundles)
        {
            bundles.Add(new ScriptBundle("~/bundles/jquery").Include(
                "~/Scripts/jquery-{version}.js"));

            bundles.Add(new ScriptBundle("~/bundles/ace").Include
                ( "~/Scripts/ace.js"
                , "~/Scripts/mode-sql.js"
                , "~/Scripts/theme-katzenmilch.js"
                , "~/Scripts/theme-sqlserver.js"
                ));

            bundles.Add(new StyleBundle("~/Content/css").Include(
                "~/Content/site.css"));
        }
    }
}
