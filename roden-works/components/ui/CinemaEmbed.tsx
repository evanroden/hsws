'use client'

import { useState, useRef } from 'react'
import { motion, AnimatePresence } from 'framer-motion'

/* ─── Types ─────────────────────────────────────── */

type VideoSource =
  | { type: 'vimeo'; id: string }
  | { type: 'youtube'; id: string }

interface CinemaEmbedProps {
  source: VideoSource
  title: string
  subtitle?: string
  /** Aspect ratio. Defaults to '16:9'. Use '2.35:1' for cinematic. */
  aspect?: '16:9' | '2.35:1' | '4:3' | '1:1'
  /** Show film-grain overlay before play */
  showGrain?: boolean
  /** Optional accent color class. Defaults to copper. */
  accentClass?: string
}

/* ─── Aspect ratio to padding-top percentage ────── */

const aspectMap: Record<string, string> = {
  '16:9': '56.25%',
  '2.35:1': '42.55%',
  '4:3': '75%',
  '1:1': '100%',
}

/* ─── Build embed URL ────────────────────────────── */

function getEmbedUrl(source: VideoSource, autoplay: boolean): string {
  if (source.type === 'vimeo') {
    const params = new URLSearchParams({
      badge: '0',
      autopause: '0',
      player_id: '0',
      app_id: '58479',
      transparent: '1',
      title: '0',
      byline: '0',
      portrait: '0',
    })
    if (autoplay) {
      params.set('autoplay', '1')
      params.set('muted', '0')
    }
    return `https://player.vimeo.com/video/${source.id}?${params.toString()}`
  }

  // YouTube
  const params = new URLSearchParams({
    rel: '0',
    modestbranding: '1',
    showinfo: '0',
  })
  if (autoplay) {
    params.set('autoplay', '1')
  }
  return `https://www.youtube.com/embed/${source.id}?${params.toString()}`
}

/* ─── Poster frame ──────────────────────────────── */

/** Real thumbnail for each video, downloaded into public/images/video-posters
 *  (YouTube maxres/hq thumbnail, Vimeo oEmbed thumbnail). */
function posterSrc(source: VideoSource): string {
  return `/images/video-posters/${source.type === 'vimeo' ? 'vimeo' : 'yt'}-${source.id}.jpg`
}

function PosterFrame({ source }: { source: VideoSource }) {
  return (
    <>
      {/* eslint-disable-next-line @next/next/no-img-element */}
      <img
        src={posterSrc(source)}
        alt=""
        loading="lazy"
        decoding="async"
        className="absolute inset-0 h-full w-full object-cover transition-transform duration-700 ease-out group-hover/cinema:scale-[1.03] motion-reduce:transition-none"
      />
      {/* Scrim keeps the title and play button legible on bright frames */}
      <div className="absolute inset-0 bg-gradient-to-t from-black/85 via-black/25 to-black/10 pointer-events-none" />
    </>
  )
}

/* ─── Main Component ─────────────────────────────── */

export default function CinemaEmbed({
  source,
  title,
  subtitle,
  aspect = '16:9',
  showGrain = true,
  accentClass = 'copper',
}: CinemaEmbedProps) {
  const [isPlaying, setIsPlaying] = useState(false)
  const containerRef = useRef<HTMLDivElement>(null)

  const paddingTop = aspectMap[aspect] || aspectMap['16:9']

  return (
    <div
      ref={containerRef}
      className="relative w-full overflow-hidden rounded-xl border border-white/10 bg-black group/cinema"
    >
      {/* Video container with aspect ratio */}
      <div className="relative" style={{ paddingTop }}>
        <AnimatePresence mode="wait">
          {!isPlaying ? (
            <motion.div
              key="poster"
              initial={{ opacity: 1 }}
              exit={{ opacity: 0 }}
              transition={{ duration: 0.4 }}
              className="absolute inset-0 flex items-center justify-center bg-gradient-to-br from-slate-950 via-slate-900 to-slate-950 cursor-pointer"
              onClick={() => setIsPlaying(true)}
            >
              <PosterFrame source={source} />

              {/* Film grain overlay */}
              {showGrain && (
                <div
                  className="absolute inset-0 opacity-[0.04] mix-blend-overlay pointer-events-none"
                  style={{
                    backgroundImage:
                      'url("data:image/svg+xml,%3Csvg viewBox=\'0 0 256 256\' xmlns=\'http://www.w3.org/2000/svg\'%3E%3Cfilter id=\'n\'%3E%3CfeTurbulence type=\'fractalNoise\' baseFrequency=\'0.9\' numOctaves=\'4\' stitchTiles=\'stitch\'/%3E%3C/filter%3E%3Crect width=\'100%25\' height=\'100%25\' filter=\'url(%23n)\'/%3E%3C/svg%3E")',
                  }}
                />
              )}

              {/* Letterbox bars for cinematic aspect */}
              {aspect === '2.35:1' && (
                <>
                  <div className="absolute top-0 left-0 right-0 h-[2%] bg-black" />
                  <div className="absolute bottom-0 left-0 right-0 h-[2%] bg-black" />
                </>
              )}

              {/* Play button */}
              <motion.button
                whileHover={{ scale: 1.08 }}
                whileTap={{ scale: 0.95 }}
                className="relative z-10 flex items-center justify-center w-20 h-20 md:w-24 md:h-24 rounded-full bg-black/40 border border-white/30 backdrop-blur-sm hover:bg-black/60 transition-all duration-300"
              >
                <svg
                  className="w-8 h-8 md:w-10 md:h-10 text-white ml-1"
                  fill="currentColor"
                  viewBox="0 0 24 24"
                >
                  <path d="M8 5v14l11-7z" />
                </svg>
              </motion.button>

              {/* Title overlay — bottom left */}
              <div className="absolute bottom-4 left-4 md:bottom-6 md:left-6 z-10">
                {subtitle && (
                  <span className="font-mono text-[10px] md:text-xs text-copper-light tracking-widest uppercase block mb-1">
                    {subtitle}
                  </span>
                )}
                <h3 className="font-serif text-sm md:text-lg text-white drop-shadow">
                  {title}
                </h3>
              </div>

              {/* Platform badge — bottom right */}
              <div className="absolute bottom-4 right-4 md:bottom-6 md:right-6 z-10">
                <span className="font-mono text-[10px] text-white/70 tracking-wider uppercase">
                  {source.type === 'vimeo' ? 'Vimeo' : 'YouTube'}
                </span>
              </div>

              {/* Aspect badge for cinematic */}
              {aspect === '2.35:1' && (
                <div className="absolute top-4 right-4 z-10">
                  <span className="font-mono text-[10px] text-faint tracking-wider">
                    2.35:1
                  </span>
                </div>
              )}
            </motion.div>
          ) : (
            <motion.div
              key="player"
              initial={{ opacity: 0 }}
              animate={{ opacity: 1 }}
              transition={{ duration: 0.4 }}
              className="absolute inset-0"
            >
              <iframe
                src={getEmbedUrl(source, true)}
                className="absolute inset-0 w-full h-full"
                allow="autoplay; fullscreen; picture-in-picture; clipboard-write; encrypted-media"
                allowFullScreen
                referrerPolicy="strict-origin-when-cross-origin"
                title={title}
              />
            </motion.div>
          )}
        </AnimatePresence>
      </div>
    </div>
  )
}

/* ─── Compact variant for grids ──────────────────── */

export function CinemaEmbedCompact({
  source,
  title,
  subtitle,
  duration,
}: {
  source: VideoSource
  title: string
  subtitle?: string
  duration?: string
}) {
  const [isPlaying, setIsPlaying] = useState(false)

  return (
    <div className="relative w-full overflow-hidden rounded-xl border border-white/10 bg-black group/cinema">
      <div className="relative" style={{ paddingTop: '56.25%' }}>
        <AnimatePresence mode="wait">
          {!isPlaying ? (
            <motion.div
              key="poster"
              exit={{ opacity: 0 }}
              transition={{ duration: 0.3 }}
              className="absolute inset-0 flex items-center justify-center bg-gradient-to-br from-slate-950 via-slate-900 to-slate-950 cursor-pointer"
              onClick={() => setIsPlaying(true)}
            >
              <PosterFrame source={source} />

              {/* Play button */}
              <motion.button
                whileHover={{ scale: 1.1 }}
                whileTap={{ scale: 0.95 }}
                className="relative z-10 flex items-center justify-center w-14 h-14 rounded-full bg-black/40 border border-white/30 backdrop-blur-sm hover:bg-black/60 transition-all duration-300"
              >
                <svg
                  className="w-6 h-6 text-white ml-0.5"
                  fill="currentColor"
                  viewBox="0 0 24 24"
                >
                  <path d="M8 5v14l11-7z" />
                </svg>
              </motion.button>

              {/* Title */}
              <div className="absolute bottom-3 left-3 z-10 max-w-[70%]">
                {subtitle && (
                  <span className="font-mono text-[9px] text-copper tracking-widest uppercase block mb-0.5">
                    {subtitle}
                  </span>
                )}
                <h4 className="font-serif text-sm text-white leading-snug truncate drop-shadow">
                  {title}
                </h4>
              </div>

              {/* Duration badge */}
              {duration && (
                <div className="absolute bottom-3 right-3 z-10 px-2 py-0.5 rounded bg-black/60 backdrop-blur-sm">
                  <span className="font-mono text-[10px] text-muted">
                    {duration}
                  </span>
                </div>
              )}
            </motion.div>
          ) : (
            <motion.div
              key="player"
              initial={{ opacity: 0 }}
              animate={{ opacity: 1 }}
              transition={{ duration: 0.3 }}
              className="absolute inset-0"
            >
              <iframe
                src={getEmbedUrl(source, true)}
                className="absolute inset-0 w-full h-full"
                allow="autoplay; fullscreen; picture-in-picture; clipboard-write; encrypted-media"
                allowFullScreen
                referrerPolicy="strict-origin-when-cross-origin"
                title={title}
              />
            </motion.div>
          )}
        </AnimatePresence>
      </div>
    </div>
  )
}
