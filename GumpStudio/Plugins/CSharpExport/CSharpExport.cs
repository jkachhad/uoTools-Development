﻿using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows.Forms;

using GumpStudio.Elements;

namespace GumpStudio.Plugins
{
	public class CSharpExport : BasePlugin
	{
		private static readonly string Template = @"
#region References
using System;

using Server;
using Server.Commands;
#endregion

namespace Server.Gumps
{
	public class ~gump_type~ : Gump
	{
		public static void Configure()
		{
			CommandSystem.Register(""~gump_type~"", AccessLevel.Administrator, e => DisplayTo(e.Mobile));
		}

		public static ~gump_type~ DisplayTo(Mobile user)
		{
			if (user == null || user.Deleted || !user.Player || user.NetState == null)
				return null;

			user.CloseGump(typeof(~gump_type~));

			var gump = new ~gump_type~(user);

			user.SendGump(gump);

			return gump;
		}

		public Mobile User { get; }

		private ~gump_type~(Mobile user) 
			: base(~gump_location~)
		{
			User = user;

			Dragable = true;
			Closable = true;
			Resizable = false;
			Disposable = false;

			AddPage(0);
			~gump_layout~
		}
		~gump_controls~
		public override void OnResponse(NetState sender, RelayInfo info)
		{
		}

		public override void OnServerClose(NetState owner)
		{
		}
	}
}
";

		private readonly Settings _Config = new Settings();

		public override BaseConfig Config => _Config;

		public override PluginInfo Info { get; } = new PluginInfo("C# Exporter", "1.1", "Vorspire", "admin@vita-nex.com", "Exports a C# file compatible with emulators targeting .NET");

		private MenuItem _MenuFileExport;

		protected override void OnLoaded()
		{
			base.OnLoaded();

			Designer.MenuFileExport.Enabled = true;

			if (_MenuFileExport == null)
			{
				_MenuFileExport = new MenuItem(".NET C#", new[]
				{
					new MenuItem("All Elements", ExportFileClick),
					new MenuItem("Selected Elements", ExportSelectionClick)
				});
			}

			Designer.MenuFileExport.MenuItems.Add(_MenuFileExport);
		}

		protected override void OnUnloaded()
		{
			base.OnUnloaded();
			
			Designer.MenuFileExport.MenuItems.Remove(_MenuFileExport);

			if (Designer.MenuFileExport.MenuItems.Count == 0)
			{
				Designer.MenuFileExport.Enabled = false;
			}
		}

		private void ExportFileClick(object sender, EventArgs e)
		{
			ExportFile(false);
		}

		private void ExportSelectionClick(object sender, EventArgs e)
		{
			ExportFile(true);
		}

		private void ExportFile(bool selected)
		{
			var fullPath = $"{Path.GetTempFileName()}.txt";

			var indent = new StringBuilder();

			var layoutBegin = Template.IndexOf("~gump_layout~");

			while (--layoutBegin >= 0)
			{
				if (Template[layoutBegin] == '\r' || Template[layoutBegin] == '\n')
				{
					break;
				}

				if (!Char.IsWhiteSpace(Template, layoutBegin))
				{
					break;
				}

				indent.Insert(0, Template[layoutBegin]);
			}

			var tabs = indent.ToString();

			var template = new StringBuilder(Template);

			template = template.Replace("~gump_type~", "CustomGump");

			var stacks = new Dictionary<GroupElement, ICSharpExportable[]>();

			if (selected)
			{
				var elements = Designer.ElementStack.SelectedElements.OfType<ICSharpExportable>().ToArray();

				if (elements.Length > 0)
				{
					stacks[Designer.ElementStack] = elements;
				}
			}
			else
			{
				foreach (var stack in Designer.Stacks)
				{
					var elements = stack.AllElements.OfType<ICSharpExportable>().ToArray();

					if (elements.Length > 0)
					{
						stacks[stack] = elements;
					}
				}
			}

			var location = Point.Empty;

			if (_Config.RelativeOffsets)
			{
				location.X = Int32.MaxValue;
				location.Y = Int32.MaxValue;

				foreach (var element in stacks.Values.SelectMany(o => o.OfType<BaseElement>()))
				{
					location.X = Math.Min(location.X, element.X);
					location.Y = Math.Min(location.Y, element.Y);
				}

				if (location.X == Int32.MaxValue)
				{
					location.X = 0;
				}

				if (location.Y == Int32.MaxValue)
				{
					location.Y = 0;
				}
			}

			template = template.Replace("~gump_location~", $"{location.X}, {location.Y}");

			var layout = new StringBuilder();

			var page = -1;

			foreach (var entry in stacks)
			{
				if (++page >= 1)
				{
					layout.AppendLine($"{tabs}AddPage({page});");
				}

				foreach (var exportable in entry.Value)
				{
					if (exportable is BaseElement element)
					{
						if (_Config.RelativeOffsets)
						{
							element.X -= location.X;
							element.Y -= location.Y;
						}

						var csharp = exportable.ToCSharpString();

						if (_Config.NoComments)
						{
							var index = csharp.IndexOf("//");

							if (index >= 0)
							{
								csharp = csharp.Substring(0, index);
							}
						}

						layout.AppendLine($"{tabs}{csharp}");

						if (_Config.RelativeOffsets)
						{
							element.X += location.X;
							element.Y += location.Y;
						}
					}
				}
			}

			template = template.Replace("~gump_layout~", layout.ToString().Trim());

			layout.Clear();
			indent.Clear();

			layoutBegin = Template.IndexOf("~gump_controls~");

			while (--layoutBegin >= 0)
			{
				if (Template[layoutBegin] == '\r' || Template[layoutBegin] == '\n')
				{
					break;
				}

				if (!Char.IsWhiteSpace(Template, layoutBegin))
				{
					break;
				}

				indent.Insert(0, Template[layoutBegin]);
			}

			tabs = indent.ToString();

			var buttonCount = 0;

			foreach (var button in stacks.Values.SelectMany(o => o.OfType<ButtonElement>().Where(b => b.ButtonType == ButtonTypeEnum.Reply)))
			{
				if (++buttonCount == 1)
				{
					layout.AppendLine();
					layout.AppendLine($"{tabs}public enum Buttons");
					layout.AppendLine($"{tabs}{{");
				}

				layout.AppendLine($"{tabs}\t{button.Name.Replace(" ", String.Empty)} = {buttonCount},");
			}

			if (buttonCount > 0)
			{
				layout.AppendLine($"{tabs}}}");
			}

			var checkCount = 0;

			foreach (var check in stacks.Values.SelectMany(o => o.OfType<CheckboxElement>()))
			{
				if (++checkCount == 1)
				{
					layout.AppendLine();
					layout.AppendLine($"{tabs}public enum Switches");
					layout.AppendLine($"{tabs}{{");
				}

				layout.AppendLine($"{tabs}\t{check.Name.Replace(" ", String.Empty)} = {checkCount},");
			}

			if (checkCount > 0)
			{
				layout.AppendLine($"{tabs}}}");
			}

			var inputCount = 0;

			foreach (var input in stacks.Values.SelectMany(o => o.OfType<TextEntryElement>()))
			{
				if (++inputCount == 1)
				{
					layout.AppendLine();
					layout.AppendLine($"{tabs}public enum Inputs");
					layout.AppendLine($"{tabs}{{");
				}

				layout.AppendLine($"{tabs}\t{input.Name.Replace(" ", String.Empty)} = {inputCount},");
			}

			if (inputCount > 0)
			{
				layout.AppendLine($"{tabs}}}");
			}

			if (layout.Length > 0)
			{
				template = template.Replace("~gump_controls~", $"{Environment.NewLine}{tabs}{layout.ToString().Trim()}{Environment.NewLine}");
			}
			else
			{
				template = template.Replace("~gump_controls~", String.Empty);
			}

			try
			{
				File.WriteAllText(fullPath, template.ToString().Trim());

				Process.Start(new ProcessStartInfo(fullPath)
				{
					UseShellExecute = true
				});
			}
			catch { }
		}

		[Serializable]
		public class Settings : BaseConfig
		{
			public override string Name => "C# Exporter";

			public bool RelativeOffsets { get; set; } = false;

			public bool NoComments { get; set; } = true;
		}
	}
}
