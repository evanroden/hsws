'use client'

import { useState } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import PageHero from '@/components/ui/PageHero'
import { useInView } from '@/lib/hooks'

const placeholderImages = [
  { id: 1, alt: 'Urban landscape study', aspect: 'aspect-[4/5]', span: '' },
  { id: 2, alt: 'Portrait series I', aspect: 'aspect-[3/4]', span: 'md:row-span-2' },
  { id: 3, alt: 'Architectural detail', aspect: 'aspect-square', span: '' },
  { id: 4, alt: 'Street photography', aspect: 'aspect-[4/5]', span: '' },
  { id: 5, alt: 'Natural light portrait', aspect: 'aspect-[3/4]', span: '' },
  { id: 6, alt: 'Environmental study', aspect: 'aspect-square', span: '' },
  { id: 7, alt: 'Documentary capture', aspect: 'aspect-[4/5]', span: 'md:row-span-2' },
  { id: 8, alt: 'Medium format landscape', aspect: 'aspect-[3/2]', span: 'md:col-span-2' },
  { id: 9, alt: 'Portrait series II', aspect: 'aspect-[4/5]', span: '' },
  { id: 10, alt: 'Abstract composition', aspect: 'aspect-square', span: '' },
  { id: 11, alt: 'Golden hour study', aspect: 'aspect-[3/4]', span: '' },
  { id: 12, alt: 'Architectural panorama', aspect: 'aspect-[16/9]', span: 'md:col-span-2' },
]

function ImageSlot({
  image,
  index,
  onOpen,
}: {
  image: (typeof placeholderImages)[0]
  index: number
  onOpen: (id: number) => void
}) {
  const { ref, isInView } = useInView(0.1)
  const [isLoaded, setIsLoaded] = useState(false)

  return (
    <motion.div
      ref={ref}
      initial={{ opacity: 0, y: 30 }}
      animate={isInView ? { opacity: 1, y: 0 } : {}}
      transition={{ duration: 0.6, delay: (index % 3) * 0.1, ease: [0.16, 1, 0.3, 1] }}
      className={`${image.span} group cursor-pointer`}
      onClick={() => onOpen(image.id)}
    >
      <div className="relative overflow-hidden rounded-lg border border-white/5 hover:border-white/20 transition-all duration-500">
        {/* Blur-up loading placeholder */}
        <div className={`${image.aspect} relative`}>
          {/* Blurred background layer */}
          <div
            className={`absolute inset-0 transition-opacity duration-700 ${
              isLoaded ? 'opacity-0' : 'opacity-100'
            }`}
            style={{
              background:
                'linear-gradient(135deg, rgba(138,155,168,0.08) 0%, rgba(11,18,21,0.9) 50%, rgba(184,115,51,0.05) 100%)',
              filter: 'blur(20px)',
              transform: 'scale(1.1)',
            }}
          />

          {/* Shimmer loading animation */}
          <div
            className={`absolute inset-0 animate-shimmer bg-gradient-to-r from-white/[0.02] via-white/[0.06] to-white/[0.02] bg-[length:200%_100%] transition-opacity duration-500 ${
              isLoaded ? 'opacity-0' : 'opacity-100'
            }`}
          />

          {/* Content area */}
          <div className="absolute inset-0 bg-gradient-to-br from-white/[0.03] to-transparent flex flex-col items-center justify-center gap-3">
            {/* Camera icon */}
            <svg
              className="w-8 h-8 text-titanium/20 group-hover:text-copper/40 transition-colors duration-500"
              fill="none"
              stroke="currentColor"
              viewBox="0 0 24 24"
            >
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={1}
                d="M3 9a2 2 0 012-2h.93a2 2 0 001.664-.89l.812-1.22A2 2 0 0110.07 4h3.86a2 2 0 011.664.89l.812 1.22A2 2 0 0018.07 7H19a2 2 0 012 2v9a2 2 0 01-2 2H5a2 2 0 01-2-2V9z"
              />
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={1}
                d="M15 13a3 3 0 11-6 0 3 3 0 016 0z"
              />
            </svg>
            <span className="font-mono text-[10px] text-titanium/30 tracking-widest uppercase">
              Image Coming Soon
            </span>
          </div>

          {/* Dot pattern */}
          <div
            className="absolute inset-0 opacity-[0.02]"
            style={{
              backgroundImage:
                'radial-gradient(circle at 1px 1px, white 1px, transparent 1px)',
              backgroundSize: '16px 16px',
            }}
          />

          {/* Hover overlay */}
          <div className="absolute inset-0 bg-copper/0 group-hover:bg-copper/5 transition-colors duration-500" />

          {/* Zoom icon on hover */}
          <div className="absolute inset-0 flex items-center justify-center opacity-0 group-hover:opacity-100 transition-opacity duration-300">
            <div className="w-10 h-10 rounded-full bg-black/40 backdrop-blur-sm flex items-center justify-center border border-white/10">
              <svg
                className="w-5 h-5 text-white"
                fill="none"
                stroke="currentColor"
                viewBox="0 0 24 24"
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={1.5}
                  d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0zM10 7v3m0 0v3m0-3h3m-3 0H7"
                />
              </svg>
            </div>
          </div>
        </div>

        {/* Caption bar */}
        <div className="px-3 py-2 bg-black/30 backdrop-blur-sm">
          <span className="font-mono text-[10px] text-titanium/50 tracking-wider">
            {String(image.id).padStart(2, '0')} &mdash; {image.alt}
          </span>
        </div>
      </div>
    </motion.div>
  )
}

function Lightbox({
  imageId,
  onClose,
}: {
  imageId: number | null
  onClose: () => void
}) {
  const image = placeholderImages.find((img) => img.id === imageId)
  if (!image) return null

  return (
    <AnimatePresence>
      {imageId !== null && (
        <motion.div
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          exit={{ opacity: 0 }}
          transition={{ duration: 0.3 }}
          className="fixed inset-0 z-50 flex items-center justify-center bg-black/95 backdrop-blur-xl"
          onClick={onClose}
        >
          {/* Close button */}
          <button
            onClick={onClose}
            className="absolute top-6 right-6 z-50 w-10 h-10 rounded-full bg-white/10 border border-white/20 flex items-center justify-center hover:bg-white/20 transition-colors"
          >
            <svg className="w-5 h-5 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M6 18L18 6M6 6l12 12" />
            </svg>
          </button>

          {/* Image area */}
          <motion.div
            initial={{ scale: 0.9, opacity: 0 }}
            animate={{ scale: 1, opacity: 1 }}
            exit={{ scale: 0.9, opacity: 0 }}
            transition={{ duration: 0.3 }}
            className="relative w-[80vw] max-w-4xl aspect-[4/3] rounded-lg overflow-hidden border border-white/10"
            onClick={(e) => e.stopPropagation()}
          >
            <div className="absolute inset-0 bg-gradient-to-br from-white/[0.03] to-transparent flex flex-col items-center justify-center gap-4">
              <svg
                className="w-16 h-16 text-titanium/15"
                fill="none"
                stroke="currentColor"
                viewBox="0 0 24 24"
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={0.5}
                  d="M4 16l4.586-4.586a2 2 0 012.828 0L16 16m-2-2l1.586-1.586a2 2 0 012.828 0L20 14m-6-6h.01M6 20h12a2 2 0 002-2V6a2 2 0 00-2-2H6a2 2 0 00-2 2v12a2 2 0 002 2z"
                />
              </svg>
              <span className="font-mono text-xs text-titanium/30 tracking-widest uppercase">
                {image.alt}
              </span>
              <span className="font-mono text-[10px] text-titanium/20 tracking-wider">
                Full resolution image coming soon
              </span>
            </div>
          </motion.div>

          {/* Navigation hint */}
          <div className="absolute bottom-8 left-1/2 -translate-x-1/2 font-mono text-xs text-titanium/30 tracking-wider">
            {String(image.id).padStart(2, '0')} / {String(placeholderImages.length).padStart(2, '0')}
          </div>
        </motion.div>
      )}
    </AnimatePresence>
  )
}

export default function PhotographyPage() {
  const [lightboxId, setLightboxId] = useState<number | null>(null)
  const gridRef = useInView(0.05)

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
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.7, delay: 0.2 }}
            className="max-w-3xl"
          >
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
          </motion.div>
        </div>
      </section>

      {/* Masonry Grid */}
      <section className="pb-section-mobile md:pb-section bg-slate-950" ref={gridRef.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0 }}
            animate={gridRef.isInView ? { opacity: 1 } : {}}
            transition={{ duration: 0.5 }}
          >
            <div className="columns-1 md:columns-2 lg:columns-3 gap-4 space-y-4">
              {placeholderImages.map((image, i) => (
                <div key={image.id} className="break-inside-avoid">
                  <ImageSlot image={image} index={i} onOpen={setLightboxId} />
                </div>
              ))}
            </div>
          </motion.div>
        </div>
      </section>

      {/* Lightbox */}
      <Lightbox imageId={lightboxId} onClose={() => setLightboxId(null)} />
    </>
  )
}
