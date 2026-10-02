import type { Metadata } from 'next'
import { listGalleryImages } from '@/lib/gallery'
import GlassArtContent from './GlassArtContent'

export const metadata: Metadata = {
  title: 'Glass Art — Fractured Futures',
  description:
    'Kiln-formed, fused glass: separate sheets of color fired together into a single surface.',
}

export default function GlassArtPage() {
  // Photos placed in public/images/glass-art appear automatically (see ASSET_GUIDE.md)
  return <GlassArtContent images={listGalleryImages('glass-art')} />
}
