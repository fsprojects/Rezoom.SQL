using System.Threading.Tasks;
using System.Web.Http;
using System.Net.Http;
using System.Text;
using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

namespace SQLFiddle.Website.Controllers
{
    public class FiddleApiController : ApiController
    {
        private static readonly JsonSerializerSettings _settings = new JsonSerializerSettings
        {
            ContractResolver = new CamelCasePropertyNamesContractResolver(),
        };
        private static HttpResponseMessage Json(object o)
        {
            return new HttpResponseMessage
            {
                Content = new StringContent
                    (JsonConvert.SerializeObject(o, _settings), Encoding.UTF8, "application/json"),
            };
        }

        [HttpPost]
        [Route("api/check")]
        public async Task<HttpResponseMessage> CheckFiddle()
        {
            var rawInput = await Request.Content.ReadAsStringAsync();
            var fiddleInput = JsonConvert.DeserializeObject<FiddleInput>(rawInput, _settings);
            var fiddleOutput = await Execution.Execute(Domain.checkFiddle(fiddleInput));
            return Json(fiddleOutput.Output);
        }

        [HttpGet]
        [Route("api/get/{id}")]
        public async Task<HttpResponseMessage> GetFiddle(string id)
        {
            var fiddleId = FiddleId.Parse(id);
            var fiddleOutput = await Execution.Execute(Domain.getFiddle(fiddleId));
            return Json(fiddleOutput);
        }

        [HttpPost]
        [Route("api/save")]
        public async Task<HttpResponseMessage> SaveFiddle()
        {
            var rawInput = await Request.Content.ReadAsStringAsync();
            var fiddleInput = JsonConvert.DeserializeObject<FiddleInput>(rawInput, _settings);
            var fiddleId = await Execution.Execute(Domain.saveFiddle(fiddleInput));
            return Json(new { id = fiddleId.ToString() });
        }

        [HttpGet]
        [Route("api/migrate")]
        public void Migrate()
        {
            Execution.Migrate();
        }
    }
}
