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

        [HttpPost]
        [Route("api/check")]
        public async Task<HttpResponseMessage> CheckFiddle()
        {
            var rawInput = await Request.Content.ReadAsStringAsync();
            var fiddleInput = JsonConvert.DeserializeObject<FiddleInput>(rawInput, _settings);
            var fiddleOutput = await Execution.Execute(Domain.checkFiddle(fiddleInput));
            return new HttpResponseMessage
            {
                Content = new StringContent
                    (JsonConvert.SerializeObject(fiddleOutput.Output, _settings), Encoding.UTF8, "application/json"),
        };
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
