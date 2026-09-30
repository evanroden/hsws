'use client'

import { useState, useRef, useEffect } from 'react'
import { motion, useScroll, useTransform } from 'framer-motion'
import Image from 'next/image'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import PageHero from '@/components/ui/PageHero'
import GalleryPending from '@/components/ui/GalleryPending'
import { useInView } from '@/lib/hooks'
import type { GalleryImage } from '@/lib/gallery'
import FiringScheduleChart from './FiringScheduleChart'

const technicalDetails = [
  {
    stage: 'Design & Layout',
    temperature: 'Room Temperature',
    duration: '2-4 hours',
    description:
      'Glass sheets are selected for color, opacity, and coefficient of expansion compatibility. Pieces are cut, ground, and arranged on a prepared kiln shelf with kiln wash separator. The design phase demands an understanding of how glass behaves under heat — colors shift, textures emerge, and adjacent pieces interact in ways that must be anticipated.',
  },
  {
    stage: 'Initial Ramp',
    temperature: '70°F to 1000°F',
    duration: '2-3 hours',
    description:
      'A controlled ramp rate of approximately 300°F per hour brings the glass slowly through the strain point. Heating too quickly risks thermal shock — the glass will crack before it ever reaches fusing temperature. Patience in this phase is structural, not optional.',
  },
  {
    stage: 'Rapid Heat',
    temperature: '1000°F to 1480°F',
    duration: '1-2 hours',
    description:
      'Once past the strain point, the ramp rate can increase. The glass transitions from rigid to plastic, softening as it approaches the fusing threshold. At 1300°F, edges begin to round. By 1480°F, separate pieces have fully merged into a single unified surface.',
  },
  {
    stage: 'Full Fuse & Soak',
    temperature: '1480°F to 1500°F',
    duration: '10-30 minutes',
    description:
      'The peak temperature determines the final texture. A tack fuse at 1380°F preserves surface texture and dimensionality. A full fuse at 1480-1500°F creates a smooth, flat surface where individual pieces become indistinguishable. A controlled soak at peak temperature ensures uniform heat distribution.',
  },
  {
    stage: 'Anneal & Cool',
    temperature: '1500°F to 960°F',
    duration: '1-2 hours',
    description:
      'The kiln crashes to the annealing point — the critical temperature where internal stress is relieved. At 960°F, the glass is held for a soak period that allows the entire piece to equalize. This is where structural integrity is determined.',
  },
  {
    stage: 'Controlled Cool-Down',
    temperature: '960°F to Room Temperature',
    duration: '8-12 hours',
    description:
      'A slow, programmed descent prevents the formation of internal stress that would cause cracking days, weeks, or months later. The kiln cools at no more than 50°F per hour through the critical strain range, then can be allowed to cool naturally to room temperature.',
  },
]

function HeroImage({ image }: { image: GalleryImage }) {
  const [zoom, setZoom] = useState(1)
  const [origin, setOrigin] = useState({ x: 50, y: 50 })
  const containerRef = useRef<HTMLDivElement>(null)

  const handleMouseMove = (e: React.MouseEvent) => {
    if (!containerRef.current) return
    const rect = containerRef.current.getBoundingClientRect()
    setOrigin({ x: ((e.clientX - rect.left) / rect.width) * 100, y: ((e.clientY - rect.top) / rect.height) * 100 })
  }

  return (
    <div
      ref={containerRef}
      className="relative w-full aspect-[16/10] overflow-hidden rounded-xl border border-white/10 cursor-zoom-in group"
      onMouseMove={handleMouseMove}
      onMouseEnter={() => setZoom(2.5)}
      onMouseLeave={() => setZoom(1)}
    >
      <div
        className="absolute inset-0 transition-transform duration-300 ease-out"
        style={{ transform: `scale(${zoom})`, transformOrigin: `${origin.x}% ${origin.y}%` }}
      >
        <Image src={image.src} alt={image.alt} fill sizes="(min-width: 1400px) 1300px, 100vw" className="object-cover" priority />
      </div>
      <div className="absolute bottom-4 right-4 opacity-0 group-hover:opacity-100 transition-opacity duration-300">
        <div className="px-3 py-1.5 rounded-full bg-black/50 backdrop-blur-sm border border-white/10">
          <span className="font-mono text-[10px] text-white/70 tracking-wider">{zoom > 1 ? `${zoom.toFixed(1)}x` : 'Hover to zoom'}</span>
        </div>
      </div>
    </div>
  )
}

function ParallaxSection({ children }: { children: React.ReactNode }) {
  const ref = useRef<HTMLDivElement>(null)
  const { scrollYProgress } = useScroll({
    target: ref,
    offset: ['start end', 'end start'],
  })
  const y = useTransform(scrollYProgress, [0, 1], [80, -80])

  return (
    <div ref={ref} className="relative overflow-hidden">
      <motion.div style={{ y }}>{children}</motion.div>
    </div>
  )
}

export default function GlassArtContent({ images }: { images: GalleryImage[] }) {
  const statementRef = useInView(0.2)
  const processRef = useInView(0.1)
  const materialsRef = useInView(0.2)

  return (
    <>
      <Breadcrumbs
        items={[
          { label: 'Studio', href: '/studio' },
          { label: 'Glass Art' },
        ]}
      />

      <PageHero
        title="Fractured Futures"
        subtitle="Kiln forming and glass fusing — exploring how fractured forms hold light, color, and meaning within a single unified surface."
        label="Glass Art"
        variant="warm"
      />

      {/* Hero image with zoom — first photo in public/images/glass-art */}
      {images.length > 0 && (
        <section className="py-section-mobile md:py-section bg-slate-950">
          <div className="content-width">
            <ParallaxSection>
              <HeroImage image={images[0]} />
            </ParallaxSection>
          </div>
        </section>
      )}

      {/* Artist Statement */}
      <section className="section-padding bg-slate-950 border-t border-white/5" ref={statementRef.ref}>
        <div className="content-width">
          <div className="grid grid-cols-1 lg:grid-cols-5 gap-12 lg:gap-20">
            <motion.div
              className="lg:col-span-2"
              initial={{ opacity: 0, y: 30 }}
              animate={statementRef.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
            >
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
                Artist Statement
              </span>
              <h2 className="font-serif text-heading text-white">On Fracture &amp; Form</h2>
            </motion.div>

            <motion.div
              className="lg:col-span-3"
              initial={{ opacity: 0, y: 30 }}
              animate={statementRef.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.7, delay: 0.15, ease: [0.16, 1, 0.3, 1] }}
            >
              <p className="text-lg text-titanium leading-relaxed">
                Glass is a material defined by contradiction. It is rigid yet fragile, transparent
                yet capable of holding color with an intensity that opaque materials cannot match.
                When fractured, glass does not degrade — it multiplies its surfaces, catches light
                from new angles, and reveals internal structures that were invisible when whole.
              </p>
              <p className="mt-6 text-titanium leading-relaxed">
                The &ldquo;Fractured Futures&rdquo; series explores this paradox as metaphor. Each piece begins
                as separate sheets of glass — distinct colors, textures, and opacities — that are
                arranged, stacked, and fired in a kiln until they fuse into a single unified form.
                The fracture lines that remain are not damage; they are the visible record of
                separate origins becoming something new. They are the seams where difference was
                not erased but integrated.
              </p>
              <p className="mt-6 text-titanium leading-relaxed">
                This work is informed by the same systems thinking that drives my engineering
                practice. Just as complex systems emerge from the interaction of simpler
                components, these glass pieces derive their visual power from the relationships
                between their constituent materials — relationships that only become visible
                through the transformative application of heat, time, and pressure.
              </p>

              {/* Pull quote */}
              <div className="mt-10 pl-6 border-l-2 border-copper/50">
                <p className="font-serif text-xl md:text-2xl text-white/90 italic leading-relaxed">
                  &ldquo;The fracture is not where the piece failed. It is where the piece began.&rdquo;
                </p>
              </div>
            </motion.div>
          </div>
        </div>
      </section>

      {/* Materials */}
      <section className="section-padding bg-gradient-to-b from-slate-950 via-copper/[0.03] to-slate-950" ref={materialsRef.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={materialsRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Materials
            </span>
            <h2 className="font-serif text-heading text-white">Medium &amp; Process</h2>
          </motion.div>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            {[
              {
                title: 'Bullseye Glass',
                detail:
                  'COE 90 compatible glass sheets in a range of transparent, opalescent, and iridescent finishes. Bullseye\'s tested compatibility system ensures that different colors and textures can be fused together without cracking from differential expansion.',
              },
              {
                title: 'Kiln Forming',
                detail:
                  'A programmable glass kiln with precise digital temperature control. Multi-segment firing schedules allow for controlled ramp rates, peak temperature holds, and annealing cycles that prevent internal stress and ensure structural longevity.',
              },
              {
                title: 'Cold Working',
                detail:
                  'After firing, pieces undergo cold working — grinding, polishing, and in some cases, sandblasting — to refine edges, adjust surface texture, and reveal internal layers. A diamond lap grinder and wet belt sander are the primary tools for this finishing stage.',
              },
            ].map((material, i) => (
              <motion.div
                key={material.title}
                initial={{ opacity: 0, y: 30 }}
                animate={materialsRef.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: i * 0.1 }}
                className="glass rounded-xl p-6 md:p-8 hover:bg-white/10 hover:border-white/20 transition-all duration-500"
              >
                <h3 className="font-mono text-sm tracking-widest uppercase text-copper mb-4">
                  {material.title}
                </h3>
                <p className="text-titanium text-sm leading-relaxed">{material.detail}</p>
              </motion.div>
            ))}
          </div>
        </div>
      </section>

      {/* Technical Process */}
      <section className="section-padding bg-slate-950 border-t border-white/5" ref={processRef.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={processRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12 md:mb-16"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Technical Process
            </span>
            <h2 className="font-serif text-heading text-white">The Firing Schedule</h2>
            <p className="mt-4 text-lg text-titanium max-w-2xl leading-relaxed">
              A kiln firing schedule is a precise sequence of temperature ramps, holds, and cooling
              stages that transforms raw glass into a fused artwork. Total cycle time: 16 to 24
              hours.
            </p>
          </motion.div>

          <FiringScheduleChart stages={technicalDetails} />
        </div>
      </section>

      {/* The work */}
      <section className="pb-section-mobile md:pb-section bg-slate-950">
        <div className="content-width">
          {images.length > 1 ? (
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
              {images.slice(1).map((img) => (
                <div key={img.src} className="relative aspect-[4/5] overflow-hidden rounded-xl border border-white/[0.06]">
                  <Image src={img.src} alt={img.alt} fill sizes="(min-width: 1024px) 33vw, (min-width: 768px) 50vw, 100vw" className="object-cover" />
                </div>
              ))}
            </div>
          ) : images.length === 0 ? (
            <GalleryPending
              title="The Fractured Futures pieces"
              body="Photography of the finished pieces is being prepared for the web. Images of the series are available on request."
              requestSubject="Fractured Futures — images request"
              secondary={{ label: 'Back to the Studio', href: '/studio' }}
            />
          ) : null}
        </div>
      </section>
    </>
  )
}
