import type { Metadata } from 'next'
import { listGalleryImages } from '@/lib/gallery'
import GlassArtContent from './GlassArtContent'
import { SourceList } from '@/components/ui/Sources'
import { GLASS_ART_SOURCES } from './sources'

export const metadata: Metadata = {
  title: 'Glass Art — Fractured Futures',
  description:
    'Kiln-formed, fused glass: separate sheets of color fired together into a single surface.',
}

export default function GlassArtPage() {
  // Photos placed in public/images/glass-art appear automatically (see ASSET_GUIDE.md)
  return (
    <>
      <GlassArtContent images={listGalleryImages('glass-art')} />
      <SourceList sources={GLASS_ART_SOURCES} />
    </>
  )
}
