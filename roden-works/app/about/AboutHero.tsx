'use client'

import { motion } from 'framer-motion'
import Image from 'next/image'
import { SITE_CONFIG } from '@/lib/constants'

/** `hasResume` is resolved at build time from public/resume.pdf (see about/page.tsx) */
export default function AboutHero({ hasResume = false }: { hasResume?: boolean }) {
  return (
    <section className="relative min-h-[70vh] flex items-end bg-slate-950 overflow-hidden">
      <div className="absolute inset-0">
        <div
          className="absolute inset-0 opacity-[0.03]"
          style={{
            backgroundImage: 'radial-gradient(circle at 1px 1px, white 1px, transparent 1px)',
            backgroundSize: '40px 40px',
          }}
        />
      </div>

      <div className="content-width w-full relative z-10 pb-16 md:pb-24 pt-32">
        <div className="grid grid-cols-1 md:grid-cols-3 gap-12 items-end">
          {/* Portrait */}
          <motion.div
            initial={{ opacity: 0, scale: 0.95 }}
            animate={{ opacity: 1, scale: 1 }}
            transition={{ duration: 0.8, ease: [0.16, 1, 0.3, 1] }}
            className="aspect-square rounded-2xl bg-gradient-to-br from-white/5 to-white/[0.02] border border-white/5 overflow-hidden"
          >
            <Image
              src="/portrait.jpg"
              alt="Evan Roden"
              width={600}
              height={600}
              className="w-full h-full object-cover"
              priority
            />
          </motion.div>

          {/* Info */}
          <div className="md:col-span-2">
            <motion.span
              initial={{ opacity: 0, y: 10 }}
              animate={{ opacity: 1, y: 0 }}
              transition={{ duration: 0.5, delay: 0.2 }}
              className="inline-block font-mono text-xs tracking-widest uppercase text-copper mb-4"
            >
              About
            </motion.span>
            <motion.h1
              initial={{ opacity: 0, y: 30 }}
              animate={{ opacity: 1, y: 0 }}
              transition={{ duration: 0.7, delay: 0.3, ease: [0.16, 1, 0.3, 1] }}
              className="font-serif text-display-xl text-white"
            >
              Evan Roden
            </motion.h1>
            <motion.p
              initial={{ opacity: 0, y: 20 }}
              animate={{ opacity: 1, y: 0 }}
              transition={{ duration: 0.7, delay: 0.5 }}
              className="mt-4 text-lg text-titanium"
            >
              Sustainability Engineer II / Asset Manager at ENFRA
            </motion.p>
            <motion.p
              initial={{ opacity: 0, y: 20 }}
              animate={{ opacity: 1, y: 0 }}
              transition={{ duration: 0.7, delay: 0.6 }}
              className="mt-1 text-sm text-muted"
            >
              Western New York
            </motion.p>
            <motion.div
              initial={{ opacity: 0, y: 20 }}
              animate={{ opacity: 1, y: 0 }}
              transition={{ duration: 0.7, delay: 0.7 }}
              className="mt-6"
            >
              {hasResume ? (
                <a
                  href="/resume.pdf"
                  download
                  className="inline-flex items-center gap-2 px-5 py-2.5 border border-copper/40 rounded-lg text-sm font-medium text-copper-light hover:bg-copper/10 hover:border-copper/60 transition-all duration-300"
                >
                  <svg className="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24" strokeWidth="1.5" aria-hidden="true">
                    <path strokeLinecap="round" strokeLinejoin="round" d="M3 16.5v2.25A2.25 2.25 0 005.25 21h13.5A2.25 2.25 0 0021 18.75V16.5M16.5 12L12 16.5m0 0L7.5 12m4.5 4.5V3" />
                  </svg>
                  Download Resume
                </a>
              ) : (
                <a
                  href={SITE_CONFIG.linkedin}
                  target="_blank"
                  rel="noopener noreferrer"
                  className="inline-flex items-center gap-2 px-5 py-2.5 border border-copper/40 rounded-lg text-sm font-medium text-copper-light hover:bg-copper/10 hover:border-copper/60 transition-all duration-300"
                >
                  <svg className="w-4 h-4" viewBox="0 0 24 24" fill="currentColor" aria-hidden="true">
                    <path d="M20.45 20.45h-3.56v-5.57c0-1.33-.02-3.04-1.85-3.04-1.85 0-2.14 1.45-2.14 2.94v5.67H9.34V9h3.42v1.56h.05c.48-.9 1.64-1.85 3.37-1.85 3.6 0 4.27 2.37 4.27 5.46v6.28zM5.34 7.43a2.06 2.06 0 110-4.13 2.06 2.06 0 010 4.13zM7.12 20.45H3.56V9h3.56v11.45zM22.22 0H1.77C.79 0 0 .77 0 1.73v20.54C0 23.23.79 24 1.77 24h20.45c.98 0 1.78-.77 1.78-1.73V1.73C24 .77 23.2 0 22.22 0z" />
                  </svg>
                  View LinkedIn profile
                </a>
              )}
            </motion.div>
          </div>
        </div>
      </div>
    </section>
  )
}
