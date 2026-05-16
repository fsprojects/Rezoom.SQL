using Newtonsoft.Json.Serialization;
using SQLFiddle;

var builder = WebApplication.CreateBuilder(args);

builder.Services
    .AddControllers()
    .AddNewtonsoftJson(opts =>
    {
        opts.SerializerSettings.ContractResolver = new CamelCasePropertyNamesContractResolver();
    });

var app = builder.Build();

app.UseDefaultFiles();
app.UseStaticFiles();
app.MapControllers();

// Apply Rezoom.SQL migrations at startup so the DB schema is ready before any
// request hits the API. Skips already-applied migrations.
Execution.Migrate();

app.Run();
