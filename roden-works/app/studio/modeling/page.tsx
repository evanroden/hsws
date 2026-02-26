'use client'

import { useState } from 'react'
import { motion } from 'framer-motion'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import PageHero from '@/components/ui/PageHero'
import { useInView } from '@/lib/hooks'

const runwayImages = [
  { id: 1, label: 'Look 1 — Opening Walk', aspect: 'aspect-[3/4]' },
  { id: 2, label: 'Look 2 — Hallway Editorial', aspect: 'aspect-[2/3]' },
  { id: 3, label: 'Look 3 — Classroom Set', aspect: 'aspect-[3/4]' },
  { id: 4, label: 'Look 4 — Locker Detail', aspect: 'aspect-[4/5]' },
  { id: 5, label: 'Look 5 — Final Walk', aspect: 'aspect-[3/4]' },
  { id: 6, label: 'Behind the Scenes', aspect: 'aspect-[4/3]' },
]

const editorialImages = [
  { id: 7, label: 'Editorial I — Full Length', aspect: 'aspect-[2/3]' },
  { id: 8, label: 'Editorial II — Portrait', aspect: 'aspect-[4/5]' },
  { id: 9, label: 'Editorial III — Detail', aspect: 'aspect-square' },
  { id: 10, label: 'Editorial IV — Environment', aspect: 'aspect-[3/4]' },
]

function FullBleedSlot({
  image,
  index,
  variant = 'default',
}: {
  image: { id: number; label: string; aspect: string }
  index: number
  variant?: 'default' | 'editorial'
}) {
  const { ref, isInView } = useInView(0.1)

  return (
    <motion.div
      ref={ref}
      initial={{ opacity: 0, y: 40 }}
      animate={isInView ? { opacity: 1, y: 0 } : {}}
      transition={{ duration: 0.8, delay: index * 0.1, ease: [0.16, 1, 0.3, 1] }}
      className="group relative"
    >
      <div
        className={`relative ${image.aspect} overflow-hidden ${
          variant === 'editorial'
            ? 'rounded-none'
            : 'rounded-lg border border-white/5 hover:border-white/15 transition-all duration-500'
        }`}
      >
        {/* Background gradient */}
        <div
          className="absolute inset-0"
          style={{
            background:
              variant === 'editorial'
                ? 'linear-gradient(160deg, rgba(11,18,21,0.95) 0%, rgba(27,58,45,0.15) 40%, rgba(11,18,21,0.98) 100%)'
                : 'linear-gradient(160deg, rgba(11,18,21,0.9) 0%, rgba(184,115,51,0.06) 50%, rgba(11,18,21,0.95) 100%)',
          }}
        />

        {/* Subtle texture */}
        <div
          className="absolute inset-0 opacity-[0.02]"
          style={{
            backgroundImage:
              'radial-gradient(circle at 1px 1px, white 1px, transparent 1px)',
            backgroundSize: '20px 20px',
          }}
        />

        {/* Center content */}
        <div className="absolute inset-0 flex flex-col items-center justify-center gap-3">
          {/* Hanger icon for fashion */}
          <svg
            className="w-10 h-10 text-titanium/15 group-hover:text-copper/25 transition-colors duration-500"
            fill="none"
            stroke="currentColor"
            viewBox="0 0 24 24"
          >
            <path
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth={0.8}
              d="M12 2C10.343 2 9 3.343 9 5c0 .729.195 1.413.536 2L3 13h18l-6.536-6C14.805 6.413 15 5.729 15 5c0-1.657-1.343-3-3-3zM3 13v2a2 2 0 002 2h14a2 2 0 002-2v-2"
            />
          </svg>
          <span className="font-mono text-[10px] text-titanium/25 tracking-widest uppercase">
            Image Coming Soon
          </span>
        </div>

        {/* Label overlay */}
        <div className="absolute bottom-0 left-0 right-0 p-4 md:p-6 bg-gradient-to-t from-black/60 to-transparent opacity-0 group-hover:opacity-100 transition-opacity duration-500">
          <span className="font-mono text-xs text-white/80 tracking-wider">{image.label}</span>
        </div>
      </div>
    </motion.div>
  )
}

export default function ModelingPage() {
  const introRef = useInView(0.2)
  const runwayRef = useInView(0.1)
  const editorialRef = useInView(0.1)
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
                Vogue Italy &mdash; 2020
              </span>
              <h1 className="font-serif text-display-xl text-white max-w-5xl">
                BizarrAudi&apos;s SchoolTime
              </h1>
              <p className="mt-6 text-lg md:text-xl text-titanium max-w-2xl leading-relaxed">
                Runway modeling for Vogue Italy&apos;s feature of BizarrAudi&apos;s SchoolTime collection &mdash; a
                bold reinterpretation of academic dress codes through high-fashion editorial
                styling.
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
              <h2 className="font-serif text-heading text-white">SchoolTime</h2>
              <p className="mt-6 text-titanium leading-relaxed">
                BizarrAudi&apos;s SchoolTime collection reimagines the rigid structures of academic
                uniforms through a contemporary fashion lens. The collection deconstructs the
                blazer, the pleated skirt, the varsity letter, and the backpack &mdash; reassembling
                them as statements of individuality rather than conformity. Oversized silhouettes
                meet tailored precision, and institutional fabrics are rendered in unexpected
                colorways.
              </p>
              <p className="mt-4 text-titanium leading-relaxed">
                Featured in Vogue Italy&apos;s 2020 coverage, the collection was recognized for its
                playful subversion of dress-code culture and its commentary on the performative
                nature of institutional identity. The runway presentation brought these themes to
                life through deliberate staging, choreography, and casting.
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
                  { label: 'Designer', value: 'BizarrAudi' },
                  { label: 'Collection', value: 'SchoolTime' },
                  { label: 'Season', value: '2020' },
                  { label: 'Role', value: 'Runway Model' },
                  { label: 'Format', value: 'Runway Show & Editorial' },
                ].map((detail) => (
                  <div
                    key={detail.label}
                    className="flex justify-between items-baseline py-3 border-b border-white/5"
                  >
                    <span className="font-mono text-xs text-titanium/60 tracking-widest uppercase">
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

      {/* Runway Images — Full Bleed Grid */}
      <section className="section-padding bg-slate-950" ref={runwayRef.ref}>
        <div className="content-width mb-12">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={runwayRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Runway
            </span>
            <h2 className="font-serif text-heading text-white">The Walk</h2>
          </motion.div>
        </div>

        {/* Full-bleed runway grid */}
        <div className="px-0 md:px-4 lg:px-0">
          <div className="grid grid-cols-2 lg:grid-cols-3 gap-1 md:gap-2">
            {runwayImages.map((image, i) => (
              <FullBleedSlot key={image.id} image={image} index={i} />
            ))}
          </div>
        </div>
      </section>

      {/* Editorial Images */}
      <section className="section-padding bg-slate-950 border-t border-white/5" ref={editorialRef.ref}>
        <div className="content-width mb-12">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={editorialRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Editorial
            </span>
            <h2 className="font-serif text-heading text-white">Off the Runway</h2>
            <p className="mt-4 text-lg text-titanium max-w-2xl leading-relaxed">
              Editorial shots captured alongside the runway presentation, exploring the collection
              in a controlled studio and environmental setting.
            </p>
          </motion.div>
        </div>

        {/* Editorial full-bleed strip */}
        <div className="w-full overflow-x-auto no-scrollbar">
          <div className="flex gap-1 md:gap-2 min-w-max px-6 md:px-8 lg:px-12">
            {editorialImages.map((image, i) => (
              <div key={image.id} className="w-[300px] md:w-[400px] lg:w-[450px] flex-shrink-0">
                <FullBleedSlot image={image} index={i} variant="editorial" />
              </div>
            ))}
          </div>
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
                &ldquo;Fashion at this level is not about clothing. It is about the body as architecture,
                the runway as stage, and the walk as performance. BizarrAudi&apos;s SchoolTime
                understood that the uniform is never neutral &mdash; it is always a statement about
                power, belonging, and the freedom to redefine both.&rdquo;
              </p>
            </div>
            <div className="mt-8 flex items-center justify-center gap-3">
              <div className="w-8 h-px bg-copper/50" />
              <span className="font-mono text-xs text-titanium/50 tracking-widest uppercase">
                Vogue Italy &mdash; 2020
              </span>
              <div className="w-8 h-px bg-copper/50" />
            </div>
          </motion.div>
        </div>
      </section>
    </>
  )
}
