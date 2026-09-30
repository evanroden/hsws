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
└── resume.pdf       # Optional — enables the "Download Resume" button
```

Galleries are **automatic**: any `.jpg`, `.png`, or `.webp` placed in
`public/images/photography/`, `public/images/glass-art/`, or `public/images/modeling/`
appears on its page at the next build, sorted by filename, with correct aspect ratios.
Until a folder has images, the page shows a "request the portfolio" panel instead of
empty placeholder tiles.

Name files with a numeric prefix to control order; the rest becomes the alt text:
`01-golden-hour-study.jpg` → alt text "Golden hour study".

## Adding Assets

### Profile Portrait
The portrait lives at `public/portrait.jpg` (square, used on the home and About pages).
Replace the file to update it — keep it square, at least 1200×1200px.

### Photography Gallery
1. Place images in `public/images/photography/` — they appear automatically
2. Recommended formats: WebP or JPEG, max 2400px on longest side
3. Clicking an image opens a full-screen viewer (arrow keys to browse, Esc to close)

### Resume
Place a PDF at `public/resume.pdf`. The About page detects it at build time and shows a
"Download Resume" button; without it, the button links to LinkedIn instead.

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
1. Place in `public/images/glass-art/` — they appear automatically
2. The first image (by filename) becomes the zoomable hero; 3000px+ recommended
3. Remaining images form a grid below the firing schedule

### Modeling/Editorial Images
1. Place in `public/images/modeling/` — they appear automatically
2. Portrait orientation (3:4 or 2:3) works best in the masonry layout

## Fonts

Fonts are self-hosted and load automatically — no action needed:

- **Newsreader** (display headings) — `app/fonts/`, loaded with `next/font/local` in `app/layout.tsx`
- **Geist Sans** (body, UI, and all numbers) and **Geist Mono** (labels) — from the `geist` package

Tailwind maps them to `font-serif`, `font-sans`, and `font-mono`.

## Contact Form

The form posts to `app/api/contact/route.ts`, which delivers mail through
[Resend](https://resend.com)'s REST API. Set these environment variables on the host:

| Variable | Purpose |
|---|---|
| `RESEND_API_KEY` | Required. Without it the form does not pretend to send — it offers visitors a pre-filled email instead. |
| `CONTACT_FROM` | Sender on a domain verified in Resend, e.g. `roden.works <contact@roden.works>` |
| `CONTACT_TO` | Recipient; defaults to the email in `lib/constants.ts` |

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
