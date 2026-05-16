using Newtonsoft.Json.Serialization;
using Rezoom;
using SQLFiddle;

var builder = WebApplication.CreateBuilder(args);

builder.Services
    .AddControllers()
    .AddNewtonsoftJson(opts =>
    {
        opts.SerializerSettings.ContractResolver = new CamelCasePropertyNamesContractResolver();
    });

// PlanExecutor is all the Rezoom wiring this app needs. ConnectionProvider is resolved
// from IConfiguration automatically (ConnectionStrings:<name> + RezoomSQL:Providers:<name>)
// unless the app explicitly registers its own ConnectionProvider.
builder.Services.AddScoped<PlanExecutor>();

var app = builder.Build();

app.UseDefaultFiles();
app.UseStaticFiles();
app.MapControllers();

// Apply Rezoom.SQL migrations at startup. Migrate takes the app's IServiceProvider
// and resolves the ConnectionProvider from it (defaulting to ConfigurationConnectionProvider).
using (var scope = app.Services.CreateScope())
{
    FiddleModel.Migrate(Rezoom.SQL.Migrations.MigrationConfig.Default, scope.ServiceProvider);
}

app.Run();
