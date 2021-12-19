﻿// Decompiled with JetBrains decompiler
// Type: GumpStudio.Elements.AlphaElement
// Assembly: GumpStudioCore, Version=1.8.3024.24259, Culture=neutral, PublicKeyToken=null
// MVID: A77D32E5-7519-4865-AA26-DCCB34429732
// Assembly location: C:\GumpStudio_1_8_R3_quinted-02\GumpStudioCore.dll

using System;
using System.Drawing;
using System.Runtime.Serialization;

namespace GumpStudio.Elements
{
	[Serializable]
	public class AlphaElement : ResizeableElement, ICSharpExportable
	{
		public override string Type => "Alpha Area";

		public AlphaElement()
		{
			_Size = new Size(100, 50);
		}

		protected AlphaElement(SerializationInfo info, StreamingContext context)
		  : base(info, context)
		{
			info.GetInt32("AlphaElementVersion");
		}

		public override void GetObjectData(SerializationInfo info, StreamingContext context)
		{
			base.GetObjectData(info, context);
			info.AddValue("AlphaElementVersion", 1);
		}

		public override void RefreshCache()
		{
		}

		public override void Render(Graphics Target)
		{
			var solidBrush = new SolidBrush(Color.FromArgb(50, Color.Red));
			Target.FillRectangle(solidBrush, Bounds);
			Target.DrawRectangle(Pens.Red, Bounds);
			solidBrush.Dispose();
		}

		public string ToCSharpString()
		{
			return $"AddAlphaRegion({X}, {Y}, {Width}, {Height});";
		}
	}
}
