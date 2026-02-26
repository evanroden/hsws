import type { Metadata } from 'next'
import PageHero from '@/components/ui/PageHero'
import StudioGallery from './StudioGallery'

export const metadata: Metadata = {
  title: 'The Studio',
  description:
    'Cinematography, photography, glass art, and editorial modeling — the creative portfolio of Evan Roden.',
}

export default function StudioPage() {
  return (
    <>
      <PageHero
        title="The Studio"
        subtitle="Cinematography, photography, kiln-formed glass art, and editorial modeling — visual storytelling across every medium."
        label="Creative Portfolio"
        variant="warm"
      />
      <StudioGallery />
    </>
  )
}
