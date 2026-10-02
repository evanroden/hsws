import type { Metadata } from 'next'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import PageHero from '@/components/ui/PageHero'
import GalleryPending from '@/components/ui/GalleryPending'
import { listGalleryImages } from '@/lib/gallery'
import PhotoGallery from './PhotoGallery'

export const metadata: Metadata = {
  title: 'Photography',
  description:
    'Medium-format and full-frame photography of architecture, people, and city streets, shot in natural light.',
}

export default function PhotographyPage() {
  // Images placed in public/images/photography appear here automatically (see ASSET_GUIDE.md)
  const images = listGalleryImages('photography')

  return (
    <>
      <Breadcrumbs
        items={[
          { label: 'Studio', href: '/studio' },
          { label: 'Photography' },
        ]}
      />

      <PageHero
        title="Photography"
        subtitle="Medium-format and full-frame photography of architecture, people, and city streets, shot in natural light."
        label="Still Images"
        variant="warm"
      />

      {/* Introduction */}
      <section className="section-padding bg-slate-950">
        <div className="content-width">
          <div className="max-w-3xl">
            <p className="text-lg text-titanium leading-relaxed">
              I shot stills before I shot motion, and composing a single frame is still the base of
              how I work with a camera. This portfolio covers architectural studies, environmental
              portraits, street photography, and abstract compositions.
            </p>
            <p className="mt-4 text-titanium leading-relaxed">
              Everything here was shot on full-frame or medium-format cameras and processed with
              minimal retouching.
            </p>
          </div>
        </div>
      </section>

      <section className="pb-section-mobile md:pb-section bg-slate-950">
        <div className="content-width">
          {images.length > 0 ? (
            <PhotoGallery images={images} />
          ) : (
            <GalleryPending
              title="Selected photographs"
              body="The photography portfolio is being curated for the web. Full-resolution selections are available on request."
              series={['Architectural studies', 'Environmental portraiture', 'Street photography', 'Abstract compositions']}
              requestSubject="Photography portfolio request"
              secondary={{ label: 'View cinematography', href: '/studio/cinematography' }}
            />
          )}
        </div>
      </section>
    </>
  )
}
