'use client'

import { motion } from 'framer-motion'

export default function AboutHero() {
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

      <div className="content-width relative z-10 pb-16 md:pb-24 pt-32">
        <div className="grid grid-cols-1 md:grid-cols-3 gap-12 items-end">
          {/* Portrait placeholder */}
          <motion.div
            initial={{ opacity: 0, scale: 0.95 }}
            animate={{ opacity: 1, scale: 1 }}
            transition={{ duration: 0.8, ease: [0.16, 1, 0.3, 1] }}
            className="aspect-[3/4] rounded-2xl bg-gradient-to-br from-white/5 to-white/[0.02] border border-white/5 overflow-hidden"
          >
            <div className="w-full h-full flex items-center justify-center">
              <div className="text-center">
                <div className="w-24 h-24 rounded-full bg-white/5 mx-auto mb-4 flex items-center justify-center">
                  <span className="font-serif text-3xl text-titanium/30">ER</span>
                </div>
                <span className="font-mono text-xs text-titanium/30 tracking-widest uppercase">
                  Portrait
                </span>
              </div>
            </div>
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
              Evan Joseph Roden
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
              className="mt-1 text-sm text-titanium/60"
            >
              Western New York
            </motion.p>
          </div>
        </div>
      </div>
    </section>
  )
}
