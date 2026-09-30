import type { Metadata } from 'next'
import { listGalleryImages } from '@/lib/gallery'
import ModelingContent from './ModelingContent'

export const metadata: Metadata = {
  title: 'Modeling — Vogue Italy',
  description: "Runway modeling for Vogue Italy's 2020 feature of BizarrAudi's SchoolTime collection.",
}

export default function ModelingPage() {
  // Photos placed in public/images/modeling appear automatically (see ASSET_GUIDE.md)
  return <ModelingContent images={listGalleryImages('modeling')} />
}
