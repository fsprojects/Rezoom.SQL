using Newtonsoft.Json.Serialization;
using Rezoom;
using Rezoom.SQL.Mapping;
using SQLFiddle;

var builder = WebApplication.CreateBuilder(args);

builder.Services
    .AddControllers()
    .AddNewtonsoftJson(opts =>
    {
        opts.SerializerSettings.ContractResolver = new CamelCasePropertyNamesContractResolver();
    });

// Rezoom.SQL ships a ready-made ConnectionProvider that resolves from IConfiguration —
// reads ConnectionStrings:{name} and RezoomSQL:Providers:{name} out of appsettings.json,
// environment variables, etc. Plug it in once, then everywhere downstream just asks for
// PlanExecutor.
builder.Services.AddSingleton<ConnectionProvider, ConfigurationConnectionProvider>();
builder.Services.AddScoped<PlanExecutor>();

var app = builder.Build();

app.UseDefaultFiles();
app.UseStaticFiles();
app.MapControllers();

// Apply Rezoom.SQL migrations at startup using the DI-registered ConnectionProvider.
using (var scope = app.Services.CreateScope())
{
    var connections = scope.ServiceProvider.GetRequiredService<ConnectionProvider>();
    FiddleModel.Migrate(Rezoom.SQL.Migrations.MigrationConfig.Default, connections);
}

app.Run();
