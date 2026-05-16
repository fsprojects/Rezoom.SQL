using Microsoft.AspNetCore.Mvc;
using Newtonsoft.Json;
using Newtonsoft.Json.Serialization;
using Rezoom;
using SQLFiddle;

namespace SQLFiddle.Website.Controllers;

[ApiController]
[Route("api")]
public class FiddleApiController(PlanExecutor planner) : ControllerBase
{
    // Newtonsoft.Json — not System.Text.Json — because the front-end JS reads F#
    // discriminated unions in the {case, fields} shape that Newtonsoft serializes
    // natively. Switching JSON libraries would require shipping a custom DU
    // converter and updating the JS at the same time.
    private static readonly JsonSerializerSettings Settings = new()
    {
        ContractResolver = new CamelCasePropertyNamesContractResolver(),
    };

    private ContentResult JsonResponse(object o) =>
        Content(JsonConvert.SerializeObject(o, Settings), "application/json");

    private static async Task<T?> ReadBody<T>(HttpRequest req)
    {
        using var reader = new StreamReader(req.Body);
        var body = await reader.ReadToEndAsync();
        return JsonConvert.DeserializeObject<T>(body, Settings);
    }

    [HttpPost("check")]
    public async Task<ContentResult> CheckFiddle()
    {
        var input = await ReadBody<FiddleInput>(Request)
            ?? throw new ArgumentException("Missing fiddle input");
        var checkedFiddle = await planner.Execute(Domain.checkFiddle(input));
        return JsonResponse(checkedFiddle.Output);
    }

    [HttpGet("get/{id}")]
    public async Task<ContentResult> GetFiddle(string id)
    {
        var fiddleId = FiddleId.Parse(id);
        var checkedFiddle = await planner.Execute(Domain.getFiddle(fiddleId));
        return JsonResponse(checkedFiddle);
    }

    [HttpPost("save")]
    public async Task<ContentResult> SaveFiddle()
    {
        var input = await ReadBody<FiddleInput>(Request)
            ?? throw new ArgumentException("Missing fiddle input");
        var fiddleId = await planner.Execute(Domain.saveFiddle(input));
        return JsonResponse(new { id = fiddleId.ToString() });
    }
}
