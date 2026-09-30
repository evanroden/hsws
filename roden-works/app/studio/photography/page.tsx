import type { Metadata } from 'next'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import PageHero from '@/components/ui/PageHero'
import GalleryPending from '@/components/ui/GalleryPending'
import { listGalleryImages } from '@/lib/gallery'
import PhotoGallery from './PhotoGallery'

export const metadata: Metadata = {
  title: 'Photography',
  description:
    'Medium-format and full-frame photography exploring architecture, portraiture, and the urban landscape through deliberate composition and natural light.',
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
        subtitle="Medium-format and full-frame photography exploring architecture, portraiture, and the urban landscape through deliberate composition and natural light."
        label="Still Images"
        variant="warm"
      />

      {/* Introduction */}
      <section className="section-padding bg-slate-950">
        <div className="content-width">
          <div className="max-w-3xl">
            <p className="text-lg text-titanium leading-relaxed">
              Photography has always been the foundation of my visual practice. Before the motion
              picture camera, there was the still frame &mdash; the discipline of composing a single image
              that holds everything within its borders. This portfolio spans architectural studies,
              environmental portraiture, street photography, and abstract compositions, each
              approached with the same intentionality and patience that defines the medium.
            </p>
            <p className="mt-4 text-titanium leading-relaxed">
              All work is shot on full-frame or medium-format systems, processed with minimal
              retouching to preserve the integrity of the original capture.
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
