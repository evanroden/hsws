'use client'

import { useState, useRef, useEffect } from 'react'
import { motion } from 'framer-motion'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import PageHero from '@/components/ui/PageHero'
import { useInView } from '@/lib/hooks'

const knownWorks = [
  {
    title: "Plato's Cave",
    year: '2022',
    role: 'Director of Photography',
    description:
      'A short narrative film exploring perception and reality through the lens of Plato\'s allegory. Shot on BlackMagic Cinema Camera 6K with anamorphic glass, graded in DaVinci Resolve for a rich, desaturated palette.',
  },
  {
    title: 'The Bridge',
    year: '2021',
    role: 'Camera Operator / Editor',
    description:
      'Documentary exploring connections between communities separated by infrastructure. Handheld verite style on the Sony a7s II, edited in Premiere Pro with sound design in After Effects.',
  },
  {
    title: 'Tulane Mask PSA',
    year: '2020',
    role: 'Director / Editor',
    description:
      'A public service announcement for Tulane University promoting mask usage during the COVID-19 pandemic. Rapid turnaround production, shot and delivered within 72 hours.',
  },
]

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

function VideoPlayer() {
  const [isPlaying, setIsPlaying] = useState(false)
  const [progress, setProgress] = useState(0)
  const [isMuted, setIsMuted] = useState(false)
  const progressRef = useRef<HTMLDivElement>(null)

  return (
    <div className="relative w-full overflow-hidden rounded-xl border border-white/10 bg-black">
      {/* Cinematic aspect ratio 2.35:1 */}
      <div className="relative" style={{ paddingTop: '42.55%' }}>
        <div className="absolute inset-0 flex items-center justify-center bg-gradient-to-br from-slate-950 via-slate-900 to-slate-950">
          {/* Film grain overlay */}
          <div
            className="absolute inset-0 opacity-[0.04] mix-blend-overlay"
            style={{
              backgroundImage:
                'url("data:image/svg+xml,%3Csvg viewBox=\'0 0 256 256\' xmlns=\'http://www.w3.org/2000/svg\'%3E%3Cfilter id=\'noise\'%3E%3CfeTurbulence type=\'fractalNoise\' baseFrequency=\'0.9\' numOctaves=\'4\' stitchTiles=\'stitch\'/%3E%3C/filter%3E%3Crect width=\'100%25\' height=\'100%25\' filter=\'url(%23noise)\'/%3E%3C/svg%3E")',
            }}
          />

          {/* Letterbox bars */}
          <div className="absolute top-0 left-0 right-0 h-[2%] bg-black" />
          <div className="absolute bottom-0 left-0 right-0 h-[2%] bg-black" />

          {/* Play button overlay */}
          {!isPlaying && (
            <button
              onClick={() => setIsPlaying(true)}
              className="relative z-10 flex items-center justify-center w-20 h-20 rounded-full bg-white/10 border border-white/20 backdrop-blur-sm hover:bg-white/20 transition-all duration-300 group"
            >
              <svg
                className="w-8 h-8 text-white ml-1 group-hover:scale-110 transition-transform"
                fill="currentColor"
                viewBox="0 0 24 24"
              >
                <path d="M8 5v14l11-7z" />
              </svg>
            </button>
          )}

          {/* Placeholder text */}
          <div className="absolute bottom-8 left-8 z-10">
            <span className="font-mono text-xs text-titanium/30 tracking-widest uppercase">
              Showreel Coming Soon
            </span>
          </div>

          {/* Anamorphic lens flare effect */}
          <div className="absolute inset-0 bg-gradient-to-r from-transparent via-copper/[0.03] to-transparent" />
        </div>
      </div>

      {/* Custom controls */}
      <div className="relative bg-black/90 backdrop-blur-sm px-4 py-3 border-t border-white/5">
        {/* Progress bar */}
        <div
          ref={progressRef}
          className="w-full h-1 bg-white/10 rounded-full mb-3 cursor-pointer group"
          onClick={(e) => {
            if (!progressRef.current) return
            const rect = progressRef.current.getBoundingClientRect()
            const pct = ((e.clientX - rect.left) / rect.width) * 100
            setProgress(Math.max(0, Math.min(100, pct)))
          }}
        >
          <div
            className="h-full bg-gradient-to-r from-copper to-copper/70 rounded-full relative transition-all duration-100"
            style={{ width: `${progress}%` }}
          >
            <div className="absolute right-0 top-1/2 -translate-y-1/2 w-3 h-3 bg-copper rounded-full opacity-0 group-hover:opacity-100 transition-opacity shadow-lg shadow-copper/30" />
          </div>
        </div>

        <div className="flex items-center justify-between">
          <div className="flex items-center gap-4">
            {/* Play/Pause */}
            <button
              onClick={() => setIsPlaying(!isPlaying)}
              className="text-white/80 hover:text-white transition-colors"
            >
              {isPlaying ? (
                <svg className="w-5 h-5" fill="currentColor" viewBox="0 0 24 24">
                  <path d="M6 19h4V5H6v14zm8-14v14h4V5h-4z" />
                </svg>
              ) : (
                <svg className="w-5 h-5" fill="currentColor" viewBox="0 0 24 24">
                  <path d="M8 5v14l11-7z" />
                </svg>
              )}
            </button>

            {/* Mute */}
            <button
              onClick={() => setIsMuted(!isMuted)}
              className="text-white/80 hover:text-white transition-colors"
            >
              {isMuted ? (
                <svg className="w-5 h-5" fill="currentColor" viewBox="0 0 24 24">
                  <path d="M16.5 12c0-1.77-1.02-3.29-2.5-4.03v2.21l2.45 2.45c.03-.2.05-.41.05-.63zm2.5 0c0 .94-.2 1.82-.54 2.64l1.51 1.51C20.63 14.91 21 13.5 21 12c0-4.28-2.99-7.86-7-8.77v2.06c2.89.86 5 3.54 5 6.71zM4.27 3L3 4.27 7.73 9H3v6h4l5 5v-6.73l4.25 4.25c-.67.52-1.42.93-2.25 1.18v2.06c1.38-.31 2.63-.95 3.69-1.81L19.73 21 21 19.73l-9-9L4.27 3zM12 4L9.91 6.09 12 8.18V4z" />
                </svg>
              ) : (
                <svg className="w-5 h-5" fill="currentColor" viewBox="0 0 24 24">
                  <path d="M3 9v6h4l5 5V4L7 9H3zm13.5 3c0-1.77-1.02-3.29-2.5-4.03v8.05c1.48-.73 2.5-2.25 2.5-4.02zM14 3.23v2.06c2.89.86 5 3.54 5 6.71s-2.11 5.85-5 6.71v2.06c4.01-.91 7-4.49 7-8.77s-2.99-7.86-7-8.77z" />
                </svg>
              )}
            </button>

            {/* Time */}
            <span className="font-mono text-xs text-titanium">0:00 / 0:00</span>
          </div>

          <div className="flex items-center gap-4">
            {/* Aspect ratio badge */}
            <span className="font-mono text-[10px] text-titanium/50 tracking-wider">2.35:1</span>

            {/* Fullscreen */}
            <button className="text-white/80 hover:text-white transition-colors">
              <svg className="w-5 h-5" fill="currentColor" viewBox="0 0 24 24">
                <path d="M7 14H5v5h5v-2H7v-3zm-2-4h2V7h3V5H5v5zm12 7h-3v2h5v-5h-2v3zM14 5v2h3v3h2V5h-5z" />
              </svg>
            </button>
          </div>
        </div>
      </div>
    </div>
  )
}

function FilmFrame({
  work,
  index,
}: {
  work: (typeof knownWorks)[0]
  index: number
}) {
  const { ref, isInView } = useInView(0.2)

  return (
    <motion.div
      ref={ref}
      initial={{ opacity: 0, x: 40 }}
      animate={isInView ? { opacity: 1, x: 0 } : {}}
      transition={{ duration: 0.7, delay: index * 0.15, ease: [0.16, 1, 0.3, 1] }}
      className="relative group"
    >
      {/* Film sprocket holes */}
      <div className="absolute -left-8 top-0 bottom-0 w-6 flex flex-col justify-between py-4">
        {Array.from({ length: 6 }).map((_, i) => (
          <div key={i} className="w-4 h-3 rounded-sm bg-white/5 border border-white/10" />
        ))}
      </div>

      <div className="glass rounded-lg overflow-hidden border-l-2 border-copper/50 group-hover:border-copper transition-colors duration-500">
        {/* Frame preview */}
        <div className="aspect-video bg-gradient-to-br from-white/[0.03] to-transparent relative overflow-hidden">
          <div
            className="absolute inset-0 opacity-[0.03]"
            style={{
              backgroundImage:
                'radial-gradient(circle at 1px 1px, white 1px, transparent 1px)',
              backgroundSize: '12px 12px',
            }}
          />
          <div className="absolute inset-0 flex items-center justify-center">
            <svg
              className="w-12 h-12 text-white/10 group-hover:text-copper/30 transition-colors duration-500"
              fill="none"
              stroke="currentColor"
              viewBox="0 0 24 24"
            >
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={1}
                d="M15 10l4.553-2.276A1 1 0 0121 8.618v6.764a1 1 0 01-1.447.894L15 14M5 18h8a2 2 0 002-2V8a2 2 0 00-2-2H5a2 2 0 00-2 2v8a2 2 0 002 2z"
              />
            </svg>
          </div>

          {/* Year badge */}
          <div className="absolute top-3 right-3 px-2 py-1 rounded bg-black/50 backdrop-blur-sm">
            <span className="font-mono text-[10px] text-copper tracking-wider">{work.year}</span>
          </div>
        </div>

        <div className="p-5 md:p-6">
          <span className="font-mono text-xs text-copper tracking-widest uppercase">
            {work.role}
          </span>
          <h3 className="font-serif text-xl md:text-2xl text-white mt-2 group-hover:text-copper transition-colors duration-300">
            {work.title}
          </h3>
          <p className="text-titanium text-sm mt-3 leading-relaxed">{work.description}</p>
        </div>
      </div>
    </motion.div>
  )
}

export default function CinematographyPage() {
  const heroRef = useInView(0.1)
  const equipRef = useInView(0.1)
  const reelRef = useInView(0.1)
  const playerRef = useInView(0.2)

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

      {/* Claiborne Avenue Productions */}
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

      {/* Video Player */}
      <section className="py-section-mobile md:py-section bg-gradient-to-b from-slate-950 via-slate-950/95 to-slate-950" ref={playerRef.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={playerRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Showreel
            </span>
            <h2 className="font-serif text-heading text-white mb-8">Selected Work</h2>
            <VideoPlayer />
          </motion.div>
        </div>
      </section>

      {/* Equipment & Post-Production */}
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
            <h2 className="font-serif text-heading text-white mb-12">Equipment &amp; Post-Production</h2>
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
                    <div key={item.name} className="border-l-2 border-white/10 pl-4 hover:border-copper/50 transition-colors duration-300">
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

      {/* Film Reel — Known Works */}
      <section className="section-padding bg-gradient-to-b from-slate-950 to-slate-950 border-t border-white/5" ref={reelRef.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={reelRef.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12 md:mb-16"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Film Reel
            </span>
            <h2 className="font-serif text-heading text-white">Known Works</h2>
            <p className="mt-4 text-lg text-titanium max-w-2xl leading-relaxed">
              A selection of narrative, documentary, and institutional projects that represent the
              range and depth of this cinematographic practice.
            </p>
          </motion.div>

          {/* Film strip decoration */}
          <div className="relative pl-8 md:pl-12">
            {/* Vertical film strip line */}
            <div className="absolute left-0 top-0 bottom-0 w-px bg-gradient-to-b from-copper/50 via-white/10 to-transparent" />

            <div className="space-y-8 md:space-y-12">
              {knownWorks.map((work, i) => (
                <FilmFrame key={work.title} work={work} index={i} />
              ))}
            </div>
          </div>
        </div>
      </section>
    </>
  )
}
