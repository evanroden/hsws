import type { Metadata } from 'next'
import { listGalleryImages } from '@/lib/gallery'
import ModelingContent from './ModelingContent'

export const metadata: Metadata = {
  title: 'Modeling — Vogue Italy',
  // Was "Runway modeling for Vogue Italy's 2020 feature", which reads as modeling for Vogue itself.
  // Aligned with the page copy. Vogue Italy coverage not found publicly: OWNER-CONFIRM.
  description: "Runway modeling in BizarrAudi's SchoolTime collection, featured by Vogue Italy in 2020.",
}

export default function ModelingPage() {
  // Photos placed in public/images/modeling appear automatically (see ASSET_GUIDE.md)
  return <ModelingContent images={listGalleryImages('modeling')} />
}
