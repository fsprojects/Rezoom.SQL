using Newtonsoft.Json.Serialization;
using Rezoom;
using Rezoom.SQL.Mapping;
using SQLFiddle;
using SQLFiddle.Website;

var builder = WebApplication.CreateBuilder(args);

builder.Services
    .AddControllers()
    .AddNewtonsoftJson(opts =>
    {
        opts.SerializerSettings.ContractResolver = new CamelCasePropertyNamesContractResolver();
    });

// Rezoom resolves plan-time dependencies (ConnectionProvider, etc.) out of the
// host's IServiceProvider, so we just register what our plans need.
builder.Services.AddSingleton<ConnectionProvider, ConfigurationConnectionProvider>();
builder.Services.AddScoped<PlanExecutor>();

var app = builder.Build();

app.UseDefaultFiles();
app.UseStaticFiles();
app.MapControllers();

// Apply Rezoom.SQL migrations at startup. Pulls the registered ConnectionProvider
// out of the request-scope container and hands it to the generated Migrate static.
using (var scope = app.Services.CreateScope())
{
    var connections = scope.ServiceProvider.GetRequiredService<ConnectionProvider>();
    FiddleModel.Migrate(Rezoom.SQL.Migrations.MigrationConfig.Default, connections);
}

app.Run();
