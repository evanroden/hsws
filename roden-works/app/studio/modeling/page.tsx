import type { Metadata } from 'next'
import { listGalleryImages } from '@/lib/gallery'
import ModelingContent from './ModelingContent'
import { SourceList } from '@/components/ui/Sources'
import { MODELING_SOURCES } from './sources'

export const metadata: Metadata = {
  title: 'Modeling — Vogue Italy',
  description: "Runway and editorial modeling for Bizar Audi's Schooltime collection, presented in Buffalo and featured in Vogue Italy in 2020.",
}

export default function ModelingPage() {
  // Photos placed in public/images/modeling appear automatically (see ASSET_GUIDE.md)
  return (
    <>
      <ModelingContent images={listGalleryImages('modeling')} />
      <SourceList sources={MODELING_SOURCES} />
    </>
  )
}
