﻿// Decompiled with JetBrains decompiler
// Type: GumpStudio.GumpDesignerMain
// Assembly: GumpStudioCore, Version=1.8.3024.24259, Culture=neutral, PublicKeyToken=null
// MVID: A77D32E5-7519-4865-AA26-DCCB34429732
// Assembly location: C:\GumpStudio_1_8_R3_quinted-02\GumpStudioCore.dll

using System;
using System.Windows.Forms;

using GumpStudio.Forms;

namespace GumpStudio
{
	internal sealed class GumpDesignerMain
	{
		[STAThread]
		public static void Main()
		{
			Application.EnableVisualStyles();
			Application.Run(new DesignerForm());
		}
	}
}
