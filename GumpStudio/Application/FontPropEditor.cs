﻿// Decompiled with JetBrains decompiler
// Type: GumpStudio.FontPropEditor
// Assembly: GumpStudioCore, Version=1.8.3024.24259, Culture=neutral, PublicKeyToken=null
// MVID: A77D32E5-7519-4865-AA26-DCCB34429732
// Assembly location: C:\GumpStudio_1_8_R3_quinted-02\GumpStudioCore.dll

using System;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Design;
using System.Security.Permissions;
using System.Windows.Forms.Design;

using Microsoft.VisualBasic.CompilerServices;

namespace GumpStudio
{
	public class FontPropEditor : UITypeEditor
	{
		protected IWindowsFormsEditorService edSvc;
		protected int ReturnValue;

		protected static Color Convert555ToARGB(short Col)
		{
			return Color.FromArgb(((short)(Col >> 10) & 31) * 8, ((short)(Col >> 5) & 31) * 8, (Col & 31) * 8);
		}

		[PermissionSet(SecurityAction.Demand, Name = "FullTrust")]
		public override object EditValue(ITypeDescriptorContext context, IServiceProvider provider, object value)
		{
			if (value.GetType() == typeof(int))
			{
				ReturnValue = Conversions.ToInteger(value);
				edSvc = (IWindowsFormsEditorService)provider.GetService(typeof(IWindowsFormsEditorService));
				if (edSvc != null)
				{
					var fontBrowser = new FontBrowser(Conversions.ToInteger(value));
					fontBrowser.ValueChanged += new FontBrowser.ValueChangedEventHandler(ValueSelected);
					edSvc.DropDownControl(fontBrowser);
					fontBrowser.Dispose();
					return ReturnValue;
				}
			}
			return value;
		}

		[PermissionSet(SecurityAction.Demand, Name = "FullTrust")]
		public override UITypeEditorEditStyle GetEditStyle(ITypeDescriptorContext context)
		{
			return UITypeEditorEditStyle.DropDown;
		}

		[PermissionSet(SecurityAction.Demand, Name = "FullTrust")]
		public override bool GetPaintValueSupported(ITypeDescriptorContext context)
		{
			return false;
		}

		protected void ValueSelected(int Value)
		{
			edSvc.CloseDropDown();
			ReturnValue = Value;
		}
	}
}
