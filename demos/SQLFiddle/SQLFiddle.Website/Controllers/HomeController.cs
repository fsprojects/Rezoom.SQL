using System.Web.Mvc;

namespace SQLFiddle.Website.Controllers
{
    public class HomeController : Controller
    {
        public ActionResult Index()
        {
            ViewBag.Title = "RZSQL Fiddle";

            return View();
        }
    }
}
