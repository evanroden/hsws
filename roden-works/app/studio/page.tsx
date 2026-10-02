import type { Metadata } from 'next'
import PageHero from '@/components/ui/PageHero'
import StudioGallery from './StudioGallery'

export const metadata: Metadata = {
  title: 'Creative Studio — Cinematography, Glass Art & Photography',
  description:
    'Cinematography, photography, glass art, and modeling by Evan Roden.',
}

export default function StudioPage() {
  return (
    <>
      <PageHero
        title="The Studio"
        subtitle="Cinematography, photography, kiln-formed glass, and runway modeling."
        label="Creative Portfolio"
        variant="warm"
      />
      <StudioGallery />
    </>
  )
}
