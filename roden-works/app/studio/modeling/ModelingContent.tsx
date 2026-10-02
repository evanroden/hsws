'use client'

import Image from 'next/image'
import { motion } from 'framer-motion'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import GalleryPending from '@/components/ui/GalleryPending'
import { useInView } from '@/lib/hooks'
import type { GalleryImage } from '@/lib/gallery'

export default function ModelingContent({ images }: { images: GalleryImage[] }) {
  const introRef = useInView(0.2)
  const runwayRef = useInView(0.1)
  const detailsRef = useInView(0.2)

  return (
    <>
      <Breadcrumbs
        items={[
          { label: 'Studio', href: '/studio' },
          { label: 'Modeling' },
        ]}
      />

      {/* Full-bleed hero — no PageHero, custom editorial layout */}
      <section className="relative min-h-screen flex items-end bg-slate-950">
        {/* Full-bleed background */}
        <div className="absolute inset-0">
          <div
            className="absolute inset-0"
            style={{
              background:
                'linear-gradient(180deg, rgba(11,18,21,0) 0%, rgba(11,18,21,0.4) 40%, rgba(11,18,21,0.95) 80%, rgba(11,18,21,1) 100%)',
            }}
          />
          <div
            className="absolute inset-0 opacity-[0.03]"
            style={{
              backgroundImage:
                'radial-gradient(circle at 1px 1px, white 1px, transparent 1px)',
              backgroundSize: '40px 40px',
            }}
          />
          {/* Fashion runway line */}
          <div className="absolute bottom-0 left-1/2 -translate-x-1/2 w-px h-[40%] bg-gradient-to-b from-transparent via-copper/20 to-copper/50" />
        </div>

        <div className="relative z-10 w-full">
          <div className="content-width pb-16 md:pb-24 pt-32">
            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={{ opacity: 1, y: 0 }}
              transition={{ duration: 0.8, delay: 0.2 }}
            >
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-6 block">
                Buffalo, NY &middot; Vogue Italy &middot; 2020
              </span>
              <h1 className="font-serif text-display-xl text-white max-w-5xl">
                Bizar Audi&apos;s Schooltime
              </h1>
              <p className="mt-6 text-lg md:text-xl text-titanium max-w-2xl leading-relaxed">
                I walked in the Buffalo runway presentation of Bizar Audi&apos;s Schooltime collection
                in 2020, and modeled in the editorial that Vogue Italy ran in its first issue of the
                COVID-19 pandemic.
              </p>
            </motion.div>
          </div>
        </div>
      </section>

      {/* Collection Details */}
      <section className="section-padding bg-slate-950 border-t border-white/5" ref={introRef.ref}>
        <div className="content-width">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-12 lg:gap-20">
            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={introRef.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
            >
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
                The Collection
              </span>
              <h2 className="font-serif text-heading text-white">Schooltime</h2>
              {/* Designer, venue, Vogue Italy feature and Evan's roles confirmed by Evan (Oct 2026). */}
              <p className="mt-6 text-titanium leading-relaxed">
                Bizar Audi is the name Austin Stoll works under. He is a multidisciplinary artist,
                designer, model, and rapper from Orchard Park, New York, now based in New York City.
                Schooltime takes pieces of the school uniform and recuts them as fashion.
              </p>
              <p className="mt-4 text-titanium leading-relaxed">
                He presented the collection in Buffalo in 2020, and Vogue Italy featured it in its
                first issue of the COVID-19 pandemic. I walked in the runway presentation and was one
                of the models in the editorial.
              </p>
            </motion.div>

            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={introRef.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.7, delay: 0.2, ease: [0.16, 1, 0.3, 1] }}
            >
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
                Credits
              </span>
              <h2 className="font-serif text-heading text-white">Details</h2>

              <div className="mt-6 space-y-4">
                {[
                  { label: 'Publication', value: 'Vogue Italy' },
                  { label: 'Designer', value: 'Bizar Audi (Austin Stoll)' },
                  { label: 'Collection', value: 'Schooltime' },
                  { label: 'Presented', value: 'Buffalo, NY · 2020' },
                  { label: 'Role', value: 'Runway & Editorial Model' },
                  { label: 'Format', value: 'Runway Presentation & Editorial' },
                ].map((detail) => (
                  <div
                    key={detail.label}
                    className="flex justify-between items-baseline py-3 border-b border-white/5"
                  >
                    <span className="font-mono text-xs text-muted tracking-widest uppercase">
                      {detail.label}
                    </span>
                    <span className="text-white font-medium">{detail.value}</span>
                  </div>
                ))}
              </div>
            </motion.div>
          </div>
        </div>
      </section>

      {/* Runway & editorial — photos from public/images/modeling */}
      <section className="section-padding bg-slate-950" ref={runwayRef.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={runwayRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">Runway &amp; Editorial</span>
            <h2 className="font-serif text-heading text-white">The Walk</h2>
          </motion.div>

          {images.length > 0 ? (
            <div className="columns-1 sm:columns-2 lg:columns-3 gap-2">
              {images.map((img) => (
                <div key={img.src} className="mb-2 break-inside-avoid overflow-hidden rounded-lg">
                  <Image
                    src={img.src}
                    alt={img.alt}
                    width={img.width}
                    height={img.height}
                    sizes="(min-width: 1024px) 33vw, (min-width: 640px) 50vw, 100vw"
                    className="w-full h-auto"
                  />
                </div>
              ))}
            </div>
          ) : (
            <GalleryPending
              title="Runway and editorial images"
              body="Photography from the Schooltime runway presentation and the accompanying editorial shoot is being prepared for the web. Images are available on request."
              series={['Runway presentation', 'Editorial']}
              requestSubject="Schooltime runway images request"
              secondary={{ label: 'Back to the Studio', href: '/studio' }}
            />
          )}
        </div>
      </section>

      {/* Closing Statement */}
      <section className="section-padding bg-slate-950 border-t border-white/5" ref={detailsRef.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={detailsRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
            className="max-w-3xl mx-auto text-center"
          >
            <div className="pl-0 border-l-0">
              <p className="font-serif text-xl md:text-2xl text-white/90 italic leading-relaxed">
                A school uniform is meant to make everyone look the same. To me, Schooltime asks who
                sets the rules for belonging.
              </p>
            </div>
            <div className="mt-8 flex items-center justify-center gap-3">
              <div className="w-8 h-px bg-copper/50" />
              <span className="font-mono text-xs text-muted tracking-widest uppercase">
                Schooltime &middot; Bizar Audi &middot; 2020
              </span>
              <div className="w-8 h-px bg-copper/50" />
            </div>
          </motion.div>
        </div>
      </section>
    </>
  )
}
