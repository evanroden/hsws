'use client'

import Image from 'next/image'
import { useCallback, useEffect, useState } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import type { GalleryImage } from '@/lib/gallery'

export default function PhotoGallery({ images }: { images: GalleryImage[] }) {
  const [open, setOpen] = useState<number | null>(null)

  const step = useCallback(
    (dir: 1 | -1) => setOpen((i) => (i === null ? i : (i + dir + images.length) % images.length)),
    [images.length]
  )

  useEffect(() => {
    if (open === null) return
    const onKey = (e: KeyboardEvent) => {
      if (e.key === 'Escape') setOpen(null)
      else if (e.key === 'ArrowRight') step(1)
      else if (e.key === 'ArrowLeft') step(-1)
    }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [open, step])

  const current = open === null ? null : images[open]

  return (
    <>
      <div className="columns-1 md:columns-2 lg:columns-3 gap-4">
        {images.map((img, i) => (
          <motion.button
            key={img.src}
            type="button"
            onClick={() => setOpen(i)}
            initial={{ opacity: 0, y: 24 }}
            whileInView={{ opacity: 1, y: 0 }}
            viewport={{ once: true, margin: '-5%' }}
            transition={{ duration: 0.6, delay: (i % 3) * 0.08, ease: [0.16, 1, 0.3, 1] }}
            className="group mb-4 block w-full break-inside-avoid overflow-hidden rounded-xl border border-white/[0.06] text-left"
            aria-label={`Open ${img.alt}`}
          >
            <Image
              src={img.src}
              alt={img.alt}
              width={img.width}
              height={img.height}
              sizes="(min-width: 1024px) 33vw, (min-width: 768px) 50vw, 100vw"
              className="w-full h-auto transition-transform duration-700 group-hover:scale-[1.02]"
            />
          </motion.button>
        ))}
      </div>

      <AnimatePresence>
        {current && (
          <motion.div
            role="dialog"
            aria-modal="true"
            aria-label={current.alt}
            initial={{ opacity: 0 }}
            animate={{ opacity: 1 }}
            exit={{ opacity: 0 }}
            className="fixed inset-0 z-[60] flex items-center justify-center bg-black/95 backdrop-blur-xl p-4 md:p-12"
            onClick={() => setOpen(null)}
          >
            <motion.div
              key={current.src}
              initial={{ opacity: 0, scale: 0.98 }}
              animate={{ opacity: 1, scale: 1 }}
              transition={{ duration: 0.3 }}
              className="relative max-h-full max-w-6xl"
              onClick={(e) => e.stopPropagation()}
            >
              <Image
                src={current.src}
                alt={current.alt}
                width={current.width}
                height={current.height}
                sizes="90vw"
                className="max-h-[82vh] w-auto rounded-lg object-contain"
                priority
              />
              <p className="mt-4 text-center font-mono text-xs text-muted">
                {String((open ?? 0) + 1).padStart(2, '0')} / {String(images.length).padStart(2, '0')} · {current.alt}
              </p>
            </motion.div>

            {[
              { dir: -1 as const, label: 'Previous image', cls: 'left-4 md:left-8', d: 'M15 19l-7-7 7-7' },
              { dir: 1 as const, label: 'Next image', cls: 'right-4 md:right-8', d: 'M9 5l7 7-7 7' },
            ].map((b) => (
              <button
                key={b.label}
                type="button"
                aria-label={b.label}
                onClick={(e) => {
                  e.stopPropagation()
                  step(b.dir)
                }}
                className={`absolute top-1/2 -translate-y-1/2 ${b.cls} flex h-11 w-11 items-center justify-center rounded-full border border-white/15 bg-white/5 text-white hover:bg-white/15 transition-colors`}
              >
                <svg className="h-5 w-5" fill="none" stroke="currentColor" strokeWidth={1.5} viewBox="0 0 24 24" aria-hidden="true">
                  <path strokeLinecap="round" strokeLinejoin="round" d={b.d} />
                </svg>
              </button>
            ))}
            <button
              type="button"
              aria-label="Close"
              onClick={() => setOpen(null)}
              className="absolute top-5 right-5 flex h-10 w-10 items-center justify-center rounded-full border border-white/15 bg-white/5 text-white hover:bg-white/15 transition-colors"
            >
              <svg className="h-5 w-5" fill="none" stroke="currentColor" strokeWidth={1.5} viewBox="0 0 24 24" aria-hidden="true">
                <path strokeLinecap="round" strokeLinejoin="round" d="M6 18L18 6M6 6l12 12" />
              </svg>
            </button>
          </motion.div>
        )}
      </AnimatePresence>
    </>
  )
}
