import type { Metadata } from 'next'
import { listGalleryImages } from '@/lib/gallery'
import GlassArtContent from './GlassArtContent'

export const metadata: Metadata = {
  title: 'Glass Art — Fractured Futures',
  description:
    'Kiln forming and glass fusing — exploring how fractured forms hold light, color, and meaning within a single unified surface.',
}

export default function GlassArtPage() {
  // Photos placed in public/images/glass-art appear automatically (see ASSET_GUIDE.md)
  return <GlassArtContent images={listGalleryImages('glass-art')} />
}
