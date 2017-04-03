using System.Threading.Tasks;
using System.Web.Http;
using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;

namespace SQLFiddle.Website.Controllers
{
    public class FiddleApiController : ApiController
    {
        [HttpPost]
        [Route("api/check")]
        public async Task<CheckedFiddle> CheckFiddle()
        {
            var settings = new JsonSerializerSettings
            {
                ContractResolver = new CamelCasePropertyNamesContractResolver(),
            };
            var example = JsonConvert.SerializeObject(new FiddleInput(FiddleBackend.SQLiteFiddle, "model", "command", true), settings);

            var rawInput = await Request.Content.ReadAsStringAsync();
            var fiddleInput = JsonConvert.DeserializeObject<FiddleInput>(rawInput, settings);
            return await Execution.Execute(Domain.checkFiddle(fiddleInput));
        }

        [HttpGet]
        [Route("api/get/{id}")]
        public async Task<CheckedFiddle> GetFiddle(string id)
        {
            var fiddleId = FiddleId.Parse(id);
            return await Execution.Execute(Domain.getFiddle(fiddleId));
        }

        [HttpPost]
        [Route("api/save")]
        public async Task<string> SaveFiddle([FromBody] FiddleInput input)
        {
            var fiddleId = await Execution.Execute(Domain.saveFiddle(input));
            return fiddleId.ToString();
        }

        [HttpGet]
        [Route("api/migrate")]
        public void Migrate()
        {
            Execution.Migrate();
        }
    }
}
