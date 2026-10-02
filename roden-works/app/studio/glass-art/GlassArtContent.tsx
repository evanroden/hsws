'use client'

import { useState, useRef, useEffect } from 'react'
import { motion, useScroll, useTransform } from 'framer-motion'
import Image from 'next/image'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import PageHero from '@/components/ui/PageHero'
import GalleryPending from '@/components/ui/GalleryPending'
import { useInView } from '@/lib/hooks'
import type { GalleryImage } from '@/lib/gallery'
import FiringScheduleChart, { type FiringStage } from './FiringScheduleChart'
import { Cite } from '@/components/ui/Sources'
import { GLASS_ART_SOURCES as S } from './sources'

const technicalDetails: FiringStage[] = [
  {
    stage: 'Design & Layout',
    temperature: 'Room Temperature',
    duration: '2-4 hours',
    description:
      'I choose glass sheets for color, opacity, and a compatible coefficient of expansion, then cut, grind, and arrange the pieces on a kiln shelf coated with kiln wash. The layout has to account for what heat will do: colors shift, textures change, and neighboring pieces flow into each other.',
  },
  // Stages 2-6 follow Bullseye's example full-fuse schedule for a 6mm lay-up:
  // https://www.bullseyeglass.com/wp-content/uploads/writing-firing-schedules-for-fusing-and-slumping.pdf
  // Previous values (960°F anneal, 1480-1500°F fuse, 16-24 h cycle) matched COE 96 glass, not Bullseye.
  {
    stage: 'Initial Ramp',
    temperature: '70°F to 1225°F',
    duration: 'About 3 hours, plus a 45-minute soak',
    description:
      'A ramp of about 400°F per hour, then a 45-minute hold at 1225°F so the heat evens out through the layers before the glass softens. Heat it too fast early on and thermal shock can crack the glass.',
    sources: ['bullseye-schedule'],
  },
  {
    stage: 'Rapid Heat',
    temperature: '1225°F to 1490°F',
    duration: 'About 30 minutes',
    description:
      'After the soak, the ramp rate can increase to about 600°F per hour. The glass softens and the stacked layers slump into one another.',
    sources: ['bullseye-schedule'],
  },
  {
    stage: 'Full Fuse & Soak',
    // Tack-fuse behavior at 1375°F: https://cdn.shopify.com/s/files/1/1725/1871/files/Glass-Tack-Fusing-Tip-Sheet.pdf
    temperature: '1490°F',
    duration: '10 minutes',
    description:
      'The peak temperature determines the final texture. A tack fuse around 1375°F bonds the pieces while keeping their height and edges. A full fuse at about 1490°F creates a smooth, flat surface where individual pieces become indistinguishable. A short soak at peak temperature lets the heat even out across the piece.',
    sources: ['bullseye-schedule', 'glacial-tack'],
  },
  {
    stage: 'Anneal & Cool',
    temperature: '1490°F to 900°F',
    duration: 'About 1.5 hours',
    description:
      'The kiln drops as fast as it can to the annealing temperature, 900°F for Bullseye glass, where internal stress is relieved. The glass is held there for an hour so the whole piece reaches the same temperature. A piece that is annealed poorly can crack later.',
    sources: ['bullseye-schedule'],
  },
  {
    stage: 'Controlled Cool-Down',
    temperature: '900°F to Room Temperature',
    duration: '2 hours, then natural cooling',
    description:
      'A slow, programmed cool keeps new stress from setting in. The kiln cools at 100°F per hour from 900°F to 700°F, then cools on its own to room temperature.',
    sources: ['bullseye-schedule'],
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
        subtitle="Kiln-formed, fused glass: separate sheets of color fired together into a single surface."
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
                Glass holds color with an intensity opaque materials can&apos;t match. When it
                fractures, it gains new surfaces that catch light from different angles and show
                internal structure that was hidden while the sheet was whole.
              </p>
              <p className="mt-6 text-titanium leading-relaxed">
                Each piece in the Fractured Futures series starts as separate sheets of glass in
                different colors, textures, and opacities. I arrange and stack them, then fire them
                in a kiln until they fuse into one form. The fracture lines that remain show where
                the separate sheets met.
              </p>
              <p className="mt-6 text-titanium leading-relaxed">
                I think about these pieces the way I think about systems in my engineering work. A
                complex system behaves the way it does because of how its parts interact, and these
                pieces get their look from how different glasses react to each other under heat
                over time.
              </p>
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
            {([
              {
                title: 'Bullseye Glass',
                // Bullseye does not rate its glass "COE 90"; it factory-tests its fusible glasses for
                // compatibility with each other. https://www.bullseyeglass.com/faq
                detail:
                  'Bullseye Compatible glass sheets (often sold as COE 90) in transparent, opalescent, and iridescent finishes. Bullseye tests its fusible glasses for compatibility with each other, so different colors and textures can be fused without cracking from uneven expansion.',
                sources: ['bullseye-faq'],
              },
              {
                title: 'Kiln Forming',
                detail:
                  'A programmable glass kiln with digital temperature control. Multi-segment schedules set the ramp rates, the hold at peak temperature, and the annealing cycle that keeps internal stress out of the finished piece.',
              },
              {
                title: 'Cold Working',
                detail:
                  'After firing, I grind, polish, and sometimes sandblast each piece to finish the edges, adjust the surface texture, and expose internal layers. Most of this is done on a diamond lap grinder and a wet belt sander.',
              },
            ] as { title: string; detail: string; sources?: string[] }[]).map((material, i) => (
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
                <p className="text-titanium text-sm leading-relaxed">
                  {material.detail}
                  {material.sources && <Cite sources={S} id={material.sources} />}
                </p>
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
              A firing schedule is the programmed sequence of temperature ramps, holds, and cooling
              stages that fuses the glass. Bullseye&apos;s reference full-fuse cycle for a 6mm piece
              runs about 12 hours.<Cite sources={S} id="bullseye-graph" />
              {/* https://www.bullseyeglass.com/wp-content/uploads/TECHBOOK_ST_idealized_firing_graph.pdf */}
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
              requestSubject="Fractured Futures images request"
              secondary={{ label: 'Back to the Studio', href: '/studio' }}
            />
          ) : null}
        </div>
      </section>
    </>
  )
}
