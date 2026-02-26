'use client'

import { motion } from 'framer-motion'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import PageHero from '@/components/ui/PageHero'
import CinemaEmbed, { CinemaEmbedCompact } from '@/components/ui/CinemaEmbed'
import { useInView } from '@/lib/hooks'

/* ─── Equipment Data ─────────────────────────────── */

const equipment = [
  {
    category: 'Camera Systems',
    items: [
      {
        name: 'BlackMagic Cinema Camera 6K',
        detail: '6K Super 35 sensor, 13 stops of dynamic range, Blackmagic RAW',
      },
      {
        name: 'Sony a7s II',
        detail: 'Full-frame mirrorless, exceptional low-light performance, S-Log2/S-Log3',
      },
    ],
  },
  {
    category: 'Post-Production',
    items: [
      { name: 'Adobe Premiere Pro', detail: 'Primary NLE for editorial assembly and delivery' },
      { name: 'Adobe After Effects', detail: 'Motion graphics, compositing, and visual effects' },
      { name: 'DaVinci Resolve', detail: 'Color grading, color science management, and finishing' },
      { name: 'Cinema Grade', detail: 'Real-time color grading directly on the footage plane' },
    ],
  },
]

/* ─── Page Component ─────────────────────────────── */

export default function CinematographyPage() {
  const heroRef = useInView(0.1)
  const reelRef = useInView(0.1)
  const narrativeRef = useInView(0.1)
  const commercialRef = useInView(0.1)
  const ambientRef = useInView(0.1)
  const audioRef = useInView(0.1)
  const funRef = useInView(0.1)
  const equipRef = useInView(0.1)

  return (
    <>
      <Breadcrumbs
        items={[
          { label: 'Studio', href: '/studio' },
          { label: 'Cinematography' },
        ]}
      />

      <PageHero
        title="Cinematography"
        subtitle="Over two decades of visual storytelling through narrative film, documentary work, and institutional content creation under the mentorship of industry professionals."
        label="Motion Pictures"
        variant="warm"
      />

      {/* Claiborne Avenue Productions + Tulane Freeman */}
      <section className="section-padding bg-slate-950" ref={heroRef.ref}>
        <div className="content-width">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-12 lg:gap-20">
            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={heroRef.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
            >
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
                Production House
              </span>
              <h2 className="font-serif text-heading text-white">
                Claiborne Avenue Productions
              </h2>
              <p className="mt-6 text-titanium leading-relaxed">
                Under the direction of Albert J. Moten, Jr., Claiborne Avenue Productions has been a
                cornerstone of professional filmmaking in New Orleans for over 20 years. Moten&apos;s credits
                span Hollywood productions including{' '}
                <span className="text-white font-medium">12 Years a Slave</span> (2013) and{' '}
                <span className="text-white font-medium">Now You See Me</span> (2013), bringing a
                level of craft and discipline that has defined the production house&apos;s approach to every
                project.
              </p>
              <p className="mt-4 text-titanium leading-relaxed">
                Working as camera operator and editor at Claiborne Avenue, I gained hands-on
                experience with professional cinema workflows &mdash; from blocking and lighting through
                to color grading and delivery. This mentorship under Moten has shaped a disciplined,
                narrative-first approach to visual storytelling that carries through every frame.
              </p>
            </motion.div>

            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={heroRef.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.7, delay: 0.2, ease: [0.16, 1, 0.3, 1] }}
            >
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
                Institutional Work
              </span>
              <h2 className="font-serif text-heading text-white">
                Tulane Freeman School
              </h2>
              <p className="mt-6 text-titanium leading-relaxed">
                As a videographer for the A.B. Freeman School of Business at Tulane University, I
                produced digital marketing content that served one of the top-ranked business schools
                in the South. This work included short-form promotional videos, faculty interviews,
                event coverage, and social media content designed for engagement across multiple
                platforms.
              </p>
              <p className="mt-4 text-titanium leading-relaxed">
                The institutional environment demanded a different kind of discipline: tight
                turnaround times, brand consistency, and the ability to tell compelling stories within
                strict creative guidelines. Every deliverable balanced the school&apos;s premium
                positioning with authentic student and faculty narratives.
              </p>
            </motion.div>
          </div>
        </div>
      </section>

      {/* ════════════════════════════════════════════════
          SHOWREEL — Full-width cinematic hero
          ════════════════════════════════════════════════ */}
      <section
        className="py-section-mobile md:py-section bg-gradient-to-b from-slate-950 via-slate-950/95 to-slate-950"
        ref={reelRef.ref}
      >
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={reelRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Showreel
            </span>
            <h2 className="font-serif text-heading text-white mb-8">Selected Work</h2>
            <CinemaEmbed
              source={{ type: 'vimeo', id: '471739161' }}
              title="Showreel"
              subtitle="Selected cinematography & editing"
              aspect="2.35:1"
            />
          </motion.div>
        </div>
      </section>

      {/* ════════════════════════════════════════════════
          NARRATIVE FILMS
          ════════════════════════════════════════════════ */}
      <section
        className="section-padding bg-slate-950 border-t border-white/5"
        ref={narrativeRef.ref}
      >
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={narrativeRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Narrative Film
            </span>
            <h2 className="font-serif text-heading text-white">Short Films</h2>
            <p className="mt-4 text-titanium max-w-2xl leading-relaxed">
              Poetic and narrative short films exploring perception, connection, and the human
              condition through cinematic language.
            </p>
          </motion.div>

          <div className="grid grid-cols-1 lg:grid-cols-2 gap-8">
            {/* The Bridge */}
            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={narrativeRef.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.1 }}
            >
              <CinemaEmbed
                source={{ type: 'vimeo', id: '491626637' }}
                title="The Bridge"
                subtitle="Poetic Short Film"
                aspect="2.35:1"
              />
              <div className="mt-4 pl-1">
                <span className="font-mono text-xs text-copper tracking-widest uppercase">
                  Camera Operator / Editor
                </span>
                <p className="text-titanium text-sm mt-2 leading-relaxed">
                  A poetic short story exploring connections between communities separated by
                  infrastructure. Handheld verite style on the Sony a7s II, edited in Premiere Pro
                  with sound design in After Effects.
                </p>
              </div>
            </motion.div>

            {/* Plato's Cave */}
            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={narrativeRef.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.2 }}
            >
              <CinemaEmbed
                source={{ type: 'vimeo', id: '491626637' }}
                title="Plato's Cave"
                subtitle="Poetic Short Film"
                aspect="2.35:1"
              />
              <div className="mt-4 pl-1">
                <span className="font-mono text-xs text-copper tracking-widest uppercase">
                  Director of Photography
                </span>
                <p className="text-titanium text-sm mt-2 leading-relaxed">
                  A short narrative exploring perception and reality through the lens of Plato&apos;s
                  allegory. Shot on BlackMagic Cinema Camera 6K with anamorphic glass, graded in
                  DaVinci Resolve for a rich, desaturated palette.
                </p>
              </div>
            </motion.div>
          </div>
        </div>
      </section>

      {/* ════════════════════════════════════════════════
          COMMERCIAL & INSTITUTIONAL
          ════════════════════════════════════════════════ */}
      <section
        className="section-padding bg-gradient-to-b from-slate-950 to-slate-950 border-t border-white/5"
        ref={commercialRef.ref}
      >
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={commercialRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Commercial Work
            </span>
            <h2 className="font-serif text-heading text-white">
              Institutional &amp; Promotional
            </h2>
            <p className="mt-4 text-titanium max-w-2xl leading-relaxed">
              Client-facing video production for museums, universities, and cultural institutions
              across Louisiana.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={commercialRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6, delay: 0.1 }}
            className="max-w-3xl"
          >
            <CinemaEmbed
              source={{ type: 'youtube', id: '7ya0DAUe5FU' }}
              title="Louisiana Children's Museum"
              subtitle="Promotional Ad"
              aspect="16:9"
            />
            <div className="mt-4 pl-1">
              <span className="font-mono text-xs text-copper tracking-widest uppercase">
                Director / Editor
              </span>
              <p className="text-titanium text-sm mt-2 leading-relaxed">
                Promotional content for the Louisiana Children&apos;s Museum, capturing the spirit
                of play-based learning and community engagement in New Orleans.
              </p>
            </div>
          </motion.div>
        </div>
      </section>

      {/* ════════════════════════════════════════════════
          AMBIENT & EXPERIMENTAL
          ════════════════════════════════════════════════ */}
      <section
        className="section-padding bg-slate-950 border-t border-white/5"
        ref={ambientRef.ref}
      >
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={ambientRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Ambient Cinema
            </span>
            <h2 className="font-serif text-heading text-white">
              4K Ambient &amp; Atmospheric
            </h2>
            <p className="mt-4 text-titanium max-w-2xl leading-relaxed">
              Long-form atmospheric video designed for immersive viewing. Shot in 4K at 60fps with
              HDR grading for natural, cinema-quality ambiance.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={ambientRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6, delay: 0.1 }}
            className="max-w-3xl"
          >
            <CinemaEmbed
              source={{ type: 'youtube', id: 'U-o0wAagNbQ' }}
              title="Afternoon in Upstate New York"
              subtitle="4K HDR &middot; 60fps &middot; 4 Hours"
              aspect="16:9"
            />
            <div className="mt-4 pl-1">
              <span className="font-mono text-xs text-copper tracking-widest uppercase">
                Cinematographer / Colorist
              </span>
              <p className="text-titanium text-sm mt-2 leading-relaxed">
                A snowy afternoon fireplace scene with jazz — designed as a living background for
                studying, working, or unwinding. Shot on location in upstate New York, graded for
                natural warmth and HDR detail.
              </p>
            </div>
          </motion.div>
        </div>
      </section>

      {/* ════════════════════════════════════════════════
          AUDIO — WWNO Classical Radio
          ════════════════════════════════════════════════ */}
      <section
        className="section-padding bg-gradient-to-b from-slate-950 to-slate-950 border-t border-white/5"
        ref={audioRef.ref}
      >
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={audioRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Audio Production
            </span>
            <h2 className="font-serif text-heading text-white">
              WWNO Classical Radio
            </h2>
            <p className="mt-4 text-titanium max-w-2xl leading-relaxed">
              Sample programming produced for WWNO, New Orleans&apos; NPR affiliate. A curated
              hour of classical music with contextual narration and seamless transitions.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={audioRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6, delay: 0.1 }}
            className="max-w-3xl"
          >
            <CinemaEmbed
              source={{ type: 'youtube', id: 'rO_H8d7LbOo' }}
              title="Classical Radio Show"
              subtitle="WWNO &middot; 1 Hour"
              aspect="16:9"
            />
          </motion.div>
        </div>
      </section>

      {/* ════════════════════════════════════════════════
          JUST FOR FUN — Elevator Reviews
          ════════════════════════════════════════════════ */}
      <section
        className="section-padding bg-slate-950 border-t border-white/5"
        ref={funRef.ref}
      >
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={funRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <div className="flex items-center gap-4 mb-4">
              <span className="font-mono text-xs tracking-widest uppercase text-copper">
                Just For Fun
              </span>
              <div className="h-px flex-1 bg-gradient-to-r from-copper/20 to-transparent" />
            </div>
            <h2 className="font-serif text-heading text-white">
              The Elevator Review Series
            </h2>
            <p className="mt-4 text-titanium max-w-2xl leading-relaxed">
              Because sometimes you just need to review an elevator. A micro-series applying
              cinema-grade production values to the world&apos;s most mundane vertical
              transportation systems.
            </p>
          </motion.div>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={funRef.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.5, delay: 0.1 }}
            >
              <CinemaEmbedCompact
                source={{ type: 'youtube', id: '_6mzmQtPKyQ' }}
                title="Elevator Review #1"
                subtitle="The Series Begins"
                duration="1:00"
              />
            </motion.div>
            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={funRef.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.5, delay: 0.2 }}
            >
              <CinemaEmbedCompact
                source={{ type: 'youtube', id: 'TV9xw4Q0eek' }}
                title="Elevator Review #2"
                subtitle="The Sequel"
                duration="1:00"
              />
            </motion.div>
            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={funRef.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.5, delay: 0.3 }}
            >
              <CinemaEmbedCompact
                source={{ type: 'youtube', id: '4NqWubbW5g4' }}
                title="Elevator Review #3"
                subtitle="The Trilogy Completes"
                duration="1:00"
              />
            </motion.div>
          </div>
        </div>
      </section>

      {/* ════════════════════════════════════════════════
          EQUIPMENT & POST-PRODUCTION
          ════════════════════════════════════════════════ */}
      <section className="section-padding bg-slate-950 border-t border-white/5" ref={equipRef.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={equipRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Technical Specifications
            </span>
            <h2 className="font-serif text-heading text-white mb-12">
              Equipment &amp; Post-Production
            </h2>
          </motion.div>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
            {equipment.map((group, gi) => (
              <motion.div
                key={group.category}
                initial={{ opacity: 0, y: 30 }}
                animate={equipRef.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: gi * 0.15 }}
                className="glass rounded-xl p-6 md:p-8"
              >
                <h3 className="font-mono text-sm tracking-widest uppercase text-copper mb-6">
                  {group.category}
                </h3>
                <div className="space-y-6">
                  {group.items.map((item) => (
                    <div
                      key={item.name}
                      className="border-l-2 border-white/10 pl-4 hover:border-copper/50 transition-colors duration-300"
                    >
                      <h4 className="text-white font-medium">{item.name}</h4>
                      <p className="text-titanium text-sm mt-1">{item.detail}</p>
                    </div>
                  ))}
                </div>
              </motion.div>
            ))}
          </div>
        </div>
      </section>
    </>
  )
}
