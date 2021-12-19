﻿// Decompiled with JetBrains decompiler
// Type: GumpStudio.Elements.ResizeableElement
// Assembly: GumpStudioCore, Version=1.8.3024.24259, Culture=neutral, PublicKeyToken=null
// MVID: A77D32E5-7519-4865-AA26-DCCB34429732
// Assembly location: C:\GumpStudio_1_8_R3_quinted-02\GumpStudioCore.dll

using System;
using System.ComponentModel;
using System.Drawing;
using System.Runtime.Serialization;

namespace GumpStudio.Elements
{
	[Serializable]
	public abstract class ResizeableElement : BaseElement
	{
		[Browsable(false)]
		public override int Height
		{
			get => _Size.Height;
			set => _Size.Height = value;
		}

		[Browsable(true)]
		public override Size Size
		{
			get => _Size;
			set => _Size = value;
		}

		[Browsable(false)]
		public override int Width
		{
			get => _Size.Width;
			set => _Size.Width = value;
		}

		public ResizeableElement()
		{
		}

		public ResizeableElement(SerializationInfo info, StreamingContext context)
		  : base(info, context)
		{
			info.GetInt32("ResizableElementVersion");
		}

		public override void DrawBoundingBox(Graphics Target, bool Active)
		{
			var bounds = Bounds;
			bounds.Inflate(3, 3);
			Brush brush;
			Pen pen;
			if (Active)
			{
				brush = new SolidBrush(Color.LightGray);
				pen = new Pen(Color.White);
			}
			else
			{
				brush = new SolidBrush(Color.Gray);
				pen = new Pen(Color.DarkGray);
			}
			base.DrawBoundingBox(Target, Active);
			bounds.Inflate(1, 1);
			Target.FillRectangle(brush, bounds.X - 2, bounds.Y - 2, 6, 6);
			Target.FillRectangle(brush, bounds.X + bounds.Width - 3, bounds.Y + bounds.Height - 3, 6, 6);
			Target.FillRectangle(brush, bounds.X + bounds.Width - 3, bounds.Y - 2, 6, 6);
			Target.FillRectangle(brush, bounds.X - 2, bounds.Y + bounds.Height - 3, 6, 6);
			Target.FillRectangle(brush, bounds.X - 2, (int)Math.Round(bounds.Y + bounds.Height / 2.0 - 2.0), 6, 6);
			Target.FillRectangle(brush, (int)Math.Round(bounds.X + bounds.Width / 2.0 - 2.0), bounds.Y - 2, 6, 6);
			Target.FillRectangle(brush, bounds.X + bounds.Width - 3, (int)Math.Round(bounds.Y + bounds.Height / 2.0 - 2.0), 6, 6);
			Target.FillRectangle(brush, (int)Math.Round(bounds.X + bounds.Width / 2.0 - 2.0), bounds.Y + bounds.Height - 3, 6, 6);
			Target.DrawRectangle(pen, bounds.X - 2, bounds.Y - 2, 6, 6);
			Target.DrawRectangle(pen, bounds.X + bounds.Width - 3, bounds.Y + bounds.Height - 3, 6, 6);
			Target.DrawRectangle(pen, bounds.X + bounds.Width - 3, bounds.Y - 2, 6, 6);
			Target.DrawRectangle(pen, bounds.X - 2, bounds.Y + bounds.Height - 3, 6, 6);
			Target.DrawRectangle(pen, bounds.X - 2, (int)Math.Round(bounds.Y + bounds.Height / 2.0 - 2.0), 6, 6);
			Target.DrawRectangle(pen, (int)Math.Round(bounds.X + bounds.Width / 2.0 - 2.0), bounds.Y - 2, 6, 6);
			Target.DrawRectangle(pen, bounds.X + bounds.Width - 3, (int)Math.Round(bounds.Y + bounds.Height / 2.0 - 2.0), 6, 6);
			Target.DrawRectangle(pen, (int)Math.Round(bounds.X + bounds.Width / 2.0 - 2.0), bounds.Y + bounds.Height - 3, 6, 6);
			brush.Dispose();
			pen.Dispose();
		}

		public override void GetObjectData(SerializationInfo info, StreamingContext context)
		{
			base.GetObjectData(info, context);
			info.AddValue("ResizableElementVersion", 1);
		}

		public override MoveType HitTest(Point Location)
		{
			var rectangle1 = Rectangle.Inflate(Bounds, 4, 4);
			var moveModeType = MoveType.None;
			if (rectangle1.Contains(Location))
			{
				moveModeType = MoveType.Move;
			}

			if (_Selected)
			{
				var rectangle2 = new Rectangle(rectangle1.X - 2, rectangle1.Y - 2, 5, 5);
				var rectangle3 = new Rectangle((int)Math.Round(rectangle1.X + rectangle1.Width / 2.0 - 2.0), rectangle1.Y - 2, 5, 5);
				var rectangle4 = new Rectangle(rectangle1.X + rectangle1.Width - 2, rectangle1.Y - 2, 5, 5);
				var rectangle5 = new Rectangle(rectangle1.X + rectangle1.Width - 2, (int)Math.Round(rectangle1.Y + rectangle1.Height / 2.0 - 2.0), 5, 5);
				var rectangle6 = new Rectangle(rectangle1.X + rectangle1.Width - 2, rectangle1.Y + rectangle1.Height - 2, 5, 5);
				var rectangle7 = new Rectangle((int)Math.Round(rectangle1.X + rectangle1.Width / 2.0 - 2.0), rectangle1.Y + rectangle1.Height - 2, 5, 5);
				var rectangle8 = new Rectangle(rectangle1.X - 2, rectangle1.Y + rectangle1.Height - 2, 5, 5);
				var rectangle9 = new Rectangle(rectangle1.X - 2, (int)Math.Round(rectangle1.Y + rectangle1.Height / 2.0 - 2.0), 5, 5);
				if (rectangle6.Contains(Location))
				{
					moveModeType = MoveType.ResizeBottomRight;
				}

				if (rectangle2.Contains(Location))
				{
					moveModeType = MoveType.ResizeTopLeft;
				}

				if (rectangle4.Contains(Location))
				{
					moveModeType = MoveType.ResizeTopRight;
				}

				if (rectangle8.Contains(Location))
				{
					moveModeType = MoveType.ResizeBottomLeft;
				}

				if (rectangle9.Contains(Location))
				{
					moveModeType = MoveType.ResizeLeft;
				}

				if (rectangle3.Contains(Location))
				{
					moveModeType = MoveType.ResizeTop;
				}

				if (rectangle5.Contains(Location))
				{
					moveModeType = MoveType.ResizeRight;
				}

				if (rectangle7.Contains(Location))
				{
					moveModeType = MoveType.ResizeBottom;
				}
			}
			return moveModeType;
		}
	}
}
