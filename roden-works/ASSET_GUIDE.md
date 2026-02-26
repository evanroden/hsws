# Asset Guide — roden.works

This document explains how to add, replace, and manage media assets for the portfolio site.

## Directory Structure

```
public/
├── models/          # 3D models (.glb, .gltf)
├── videos/          # Video files (.mp4, .webm)
├── images/          # Photography, art, portraits
│   ├── portrait/    # Professional headshots
│   ├── photography/ # Photo gallery images
│   ├── glass-art/   # Fractured Futures images
│   ├── modeling/    # Vogue/runway images
│   ├── projects/    # Case study imagery
│   └── press/       # Media logos
└── fonts/           # Custom font files (.woff2)
```

## Adding Assets

### Profile Portrait
1. Place your portrait image in `public/images/portrait/`
2. Recommended: 800x1067px (3:4 ratio), JPEG or WebP
3. Update the portrait placeholder in `app/about/AboutHero.tsx` with:
   ```tsx
   import Image from 'next/image'
   <Image src="/images/portrait/your-photo.jpg" alt="Evan Roden" fill className="object-cover" />
   ```

### Photography Gallery
1. Place images in `public/images/photography/`
2. Recommended formats: WebP or JPEG, max 2400px on longest side
3. Add entries to the gallery array in `app/studio/photography/page.tsx`
4. Include EXIF data if available (camera, lens, settings)

### 3D Models (VA Prosthetics)
1. Export from Fusion 360 as .glb (binary glTF)
2. Place in `public/models/`
3. Optimize with [gltf-transform](https://gltf-transform.dev/): `npx @gltf-transform/cli optimize input.glb output.glb`
4. Keep models under 5MB for fast loading
5. Update the model path in `components/three/ProstheticViewer.tsx`

### Videos
1. Place in `public/videos/`
2. Recommended: MP4 (H.264), 1080p, AAC audio
3. Create a poster image (first frame) at the same location with `-poster.jpg` suffix
4. Update the video player in `app/studio/cinematography/page.tsx`

### Glass Art Images
1. Place in `public/images/glass-art/`
2. High-resolution recommended (3000px+) for zoom feature
3. Update gallery in `app/studio/glass-art/page.tsx`

### Modeling/Editorial Images
1. Place in `public/images/modeling/`
2. Full-bleed layout works best with portrait orientation (3:4 or 2:3)
3. Update `app/studio/modeling/page.tsx`

## Fonts

The site uses system font fallbacks by default. To use custom fonts:

1. Download font files (.woff2 format):
   - [Inter](https://fonts.google.com/specimen/Inter) — body text
   - [Playfair Display](https://fonts.google.com/specimen/Playfair+Display) — headings
   - [JetBrains Mono](https://fonts.google.com/specimen/JetBrains+Mono) — monospace/data

2. Place in `public/fonts/` as `inter-variable.woff2`, `playfair-variable.woff2`, `jetbrains-mono-variable.woff2`

3. Update `app/layout.tsx` to use `next/font/local`:
   ```tsx
   import localFont from 'next/font/local'
   const sans = localFont({ src: '../public/fonts/inter-variable.woff2', variable: '--font-inter' })
   ```

4. Update `tailwind.config.ts` font families to use CSS variables:
   ```ts
   fontFamily: {
     serif: ['var(--font-playfair)', 'Georgia', 'serif'],
     sans: ['var(--font-inter)', 'system-ui', 'sans-serif'],
     mono: ['var(--font-jetbrains)', 'monospace'],
   }
   ```

## Contact Form

The contact form currently logs submissions to the server console. To wire it up to email:

1. Install Resend: `npm install resend`
2. Get an API key from [resend.com](https://resend.com)
3. Add `RESEND_API_KEY` to your environment variables
4. Update `app/api/contact/route.ts` to use the Resend SDK

## MDX Case Studies

To convert any case study to MDX for richer content:

1. Create a `.mdx` file in `content/`
2. Install `next-mdx-remote`: `npm install next-mdx-remote`
3. Use `MDXRemote` in the page component to render

## Deployment

The site is configured for static export by default. Deploy to:

- **Vercel**: `npx vercel` (recommended — zero config)
- **Netlify**: Push to Git, connect repo
- **Custom**: `npm run build && npm run start`

### Custom Domain (roden.works)
Configure DNS:
- A record: `@` → Vercel/Netlify IP
- CNAME record: `www` → deployment URL

## Image Optimization

All images should use `next/image` for automatic:
- WebP/AVIF conversion
- Responsive sizing
- Lazy loading
- Blur-up placeholders

When adding new images, prefer WebP format and keep originals under 1MB.
