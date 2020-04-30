---
metaTitle: ".NET Compiler Platform (Roslyn)"
description: "Semantic model, Syntax tree, Create workspace from MSBuild project"
---

# .NET Compiler Platform (Roslyn)




## Semantic model


A **Semantic Model** offers a deeper level of interpretation and insight of code compare to a syntax tree. Where syntax trees can tell the names of variables, semantic models also give the type and all references. Syntax trees notice method calls, but semantic models give references to the precise location the method is declared (after overload resolution has been applied.)

```cs
var workspace = Microsoft.CodeAnalysis.MSBuild.MSBuildWorkspace.Create();
var sln = await workspace.OpenSolutionAsync(solutionFilePath);
var project = sln.Projects.First();
var compilation = await project.GetCompilationAsync();

foreach (var syntaxTree in compilation.SyntaxTrees)
{
    var root = await syntaxTree.GetRootAsync();

    var declaredIdentifiers = root.DescendantNodes()
        .Where(an => an is VariableDeclaratorSyntax)
        .Cast<VariableDeclaratorSyntax>();

    foreach (var di in declaredIdentifiers)
    {
        Console.WriteLine(di.Identifier);
        // => "root"

        var variableSymbol = compilation
            .GetSemanticModel(syntaxTree)
            .GetDeclaredSymbol(di) as ILocalSymbol;

        Console.WriteLine(variableSymbol.Type);
        // => "Microsoft.CodeAnalysis.SyntaxNode"

        var references = await SymbolFinder.FindReferencesAsync(variableSymbol, sln);
        foreach (var reference in references)
        {
            foreach (var loc in reference.Locations)
            {
                Console.WriteLine(loc.Location.SourceSpan);
                // => "[1375..1379)"
            }
        }
    }
}

```

This outputs a list of local variables using a syntax tree. Then it consults the semantic model to get the full type name and find all references of every variable.



## Syntax tree


A **Syntax Tree** is an immutable data structure representing the program as a tree of names, commands and marks (as previously configured in the editor.)

For example, assume a `Microsoft.CodeAnalysis.Compilation` instance named `compilation` has been configured. There are multiple ways to list the names of every variable declared in the loaded code. To do so naively, take all pieces of syntax in every document (the `DescendantNodes` method) and use Linq to select nodes that describe variable declaration:

```cs
foreach (var syntaxTree in compilation.SyntaxTrees)
{
    var root = await syntaxTree.GetRootAsync();
    var declaredIdentifiers = root.DescendantNodes()
        .Where(an => an is VariableDeclaratorSyntax)
        .Cast<VariableDeclaratorSyntax>()
        .Select(vd => vd.Identifier);

    foreach (var di in declaredIdentifiers)
    {
        Console.WriteLine(di);
    }
}

```

Every type of C# construct with a corresponding type will exist in the syntax tree. To quickly find specific types, use the `Syntax Visualizer` window from Visual Studio. This will interpret the current opened document as a Roslyn syntax tree.



## Create workspace from MSBuild project


First obtain the `Microsoft.CodeAnalysis.CSharp.Workspaces` nuget before continuing.

```cs
var workspace = Microsoft.CodeAnalysis.MSBuild.MSBuildWorkspace.Create();
var project = await workspace.OpenProjectAsync(projectFilePath);
var compilation = await project.GetCompilationAsync();

foreach (var diagnostic in compilation.GetDiagnostics()
    .Where(d => d.Severity == Microsoft.CodeAnalysis.DiagnosticSeverity.Error))
{
    Console.WriteLine(diagnostic);
}

```

To load existing code to the workspace, compile and report errors. Afterwards the code will be located in memory. From here, both the syntactic and semantic side will be available to work with.

