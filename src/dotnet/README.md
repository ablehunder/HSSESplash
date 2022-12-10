# ![l](../../res/HSSESplash.ico) HSSESplash in C#

## Compile/Build, Run, Publish
This application is ported into csharp language using vscode (because I don't and cannot have VS installed in my laptop even VS Community Edition). So you'd probably found error opening this source in VS. Some features were added using capabilities of .NET Framework.

You might try to edit <code>HSSESplash.csproj</code> as necessary. Pay attention to element <code>SelfContained</code> and <code>RuntimeIdentifier</code> to satisfy your development mode to build, run/debug, or publish the application

- Build the source code

        dotnet build -c Debug
        dotnet build -c Release
    
- Running the application. *Console* is activated on **Debug** profile, but deactivated on **Release**.

        HSSESplash.exe
        
    or
        
        dotnet run -sc -c Debug
    
- Publish the package. We use **self-contained** to publish the  runtime package according to *runtime identifier* specified in <code>HSSESplash.csproj</code>.

        dotnet publish -sc -c Release

See further help on [dotnet documentation](https://learn.microsoft.com/en-us/dotnet/core/tools/dotnet) (or simply type `dotnet -h` in command line prompt).<br>Or refer to [deployment documentation](https://learn.microsoft.com/en-us/visualstudio/deployment/?view=vs-2022) if you wish to use Visual Studio.

## Configure the application & XML Configuration Tag
This application has ***hot configuration reload*** feature, which mean, some of these options can be changed while application is active and reloaded without having to restart. It also can be configured using arguments in command line.

See corresponding setting inside file `HSSESplash.XML` example. The explanation can be found in [Configuration File](../../README.md#ConfigurationFile) section.

## Installer / Deployment Package
We can have many deploy scenarios to match your purpose. Each individual can have different configuration. For example, staff have `maxCancel=2` therefore cannot skip more than twice, but manager have `maxCancel=0` so they can always skip it.

We don't need to hard-coded configuration then compile and rebuild, instead we can update the files below and publish them into our installer / deployment package.

On the other hand, we can deploy these files separately from the installer. So we can have single installer, but another deployment/installer to update these files to any machine/user.

- Icon `HSSESplash.ico`
    > It will be used as icon in the form and tray. You might modify and deploy it as you wish without changing/recompile the source code.
    <br> You may prefer to change it using icon of your company logo. Just put your favorite icon at the same directory of this application with this file name.
- XML File `HSSESplash.xml`
    > Take a look at the example and [Configuration File](../../README.md#ConfigurationFile) section.
- HTML content `randomredirect.html`
    > Update this file using your favourite editor, you will find *javascript code* inside, and then you may add some media that will be played in random.
    <br>Or, you may replace it with your other start splash page.
    <br>Or, you don't want to use it and change your `url` setting in `HSSESplash.xml` to somewhere else URI (e.g. youtube or internal server?).
- Image/video content
    > We don't recommend to deploy your contents with application package.
    <br> Consider to have different approach, such as use separate deployment script for the content and change your content in `randomredirect.html`, or use *url* in `HSSESplash.xml` to point to somewhere.

Feel free to drop me a message.

Good Luck!
