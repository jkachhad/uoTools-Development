using System;
using System.Collections.Generic;
using Cyotek.Drawing.BitmapFont;
using System.Linq;
using System.Reflection;

#if !XENKO
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
#else
using Xenko.Core.Mathematics;
using Xenko.Graphics;
using Xenko.Graphics.Font;
using Texture2D = Xenko.Graphics.Texture;
#endif

namespace SpriteFontPlus
{
#if BF2SF_INTERNAL
	internal
#else
	public
#endif
	class TextureWithOffset
	{
		public Texture2D Texture { get; set; }
		public Point Offset { get; set; }

		public TextureWithOffset(Texture2D texture)
		{
			if (texture == null)
			{
				throw new ArgumentNullException("texture");
			}

			Texture = texture;
		}

		public TextureWithOffset(Texture2D texture, Point offset): this(texture)
		{
			Offset = offset;
		}
	}

#if BF2SF_INTERNAL
	internal
#else
	public
#endif
	static class BMFontLoader
	{
#if XENKO
		private static FontSystem _fontSystem;

		public static FontSystem FontSystem
		{
			get
			{
				if (_fontSystem != null)
				{
					return _fontSystem;
				}

				_fontSystem = new FontSystem();
				FontSystem.Load(GraphicsDevice, null);

				return _fontSystem;
			}
		}

		public static GraphicsDevice GraphicsDevice;
#endif

		private static SpriteFont Load(BitmapFont data, Func<string, TextureWithOffset> textureGetter)
		{
			if (data.Pages.Length > 1)
			{
				throw new NotSupportedException("For now only BMFonts with single texture are supported");
			}

			var texture = textureGetter(data.Pages[0].FileName);

#if !XENKO
			var glyphBounds = new List<Rectangle>();
			var cropping = new List<Rectangle>();
			var chars = new List<char>();
			var kerning = new List<Vector3>();

			var characters = data.Characters.Values.OrderBy(c => c.Char);
			foreach (var character in characters)
			{
				var bounds = character.Bounds;

				bounds.Offset(texture.Offset);
				glyphBounds.Add(bounds);
				cropping.Add(new Rectangle(character.Offset.X, character.Offset.Y, bounds.Width, bounds.Height));

				chars.Add(character.Char);

				kerning.Add(new Vector3(0, character.Bounds.Width, character.XAdvance - character.Bounds.Width));
			}

			var constructorInfo = typeof(SpriteFont).GetTypeInfo().DeclaredConstructors.First();
			var result = (SpriteFont)constructorInfo.Invoke(new object[]
			{
				texture.Texture, glyphBounds, cropping,
				chars, data.LineHeight, 0, kerning, ' '
			});

			return result;
#else
			var textureRegion = textureGetter(data.Pages[0].FileName);

			var glyphs = new List<Glyph>();
			foreach (var pair in data.Characters)
			{
				var character = pair.Value;

				var bounds = character.Bounds;
				bounds.X += textureRegion.Offset.X;
				bounds.Y += textureRegion.Offset.Y;
				var glyph = new Glyph
				{
					Character = character.Char,
					BitmapIndex = 0,
					Offset = new Vector2(character.Offset.X, character.Offset.Y),
					Subrect = bounds,
					XAdvance = character.XAdvance
				};

				glyphs.Add(glyph);
			}

			var textures = new List<Texture2D>
			{
				textureRegion.Texture
			};

			return FontSystem.NewStatic(data.LineHeight, glyphs, textures, 0, data.LineHeight);
#endif
		}

		public static SpriteFont LoadXml(string xml,
			Func<string, TextureWithOffset> textureGetter)
		{
			var data = new BitmapFont();
			data.LoadXml(xml);

			return Load(data, textureGetter);
		}

		public static SpriteFont LoadText(string xml,
			Func<string, TextureWithOffset> textureGetter)
		{
			var data = new BitmapFont();
			data.LoadText(xml);

			return Load(data, textureGetter);
		}

		public static SpriteFont Load(string str,
			Func<string, TextureWithOffset> textureGetter)
		{
			var data = new BitmapFont();
			if (str.StartsWith("<"))
			{
				// xml
				data.LoadXml(str);
			}
			else
			{
				data.LoadText(str);
			}

			return Load(data, textureGetter);
		}
	}
}