'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'
import CinemaEmbed from '@/components/ui/CinemaEmbed'

export default function VideoFeature() {
  const { ref, isInView } = useInView(0.1)

  return (
    <section
      className="py-section-mobile md:py-section bg-gradient-to-b from-slate-950 via-copper/[0.02] to-slate-950 border-t border-white/5"
      ref={ref}
    >
      <div className="content-width">
        <motion.div
          initial={{ opacity: 0, y: 30 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
        >
          <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
            Watch
          </span>
          <h2 className="font-serif text-heading text-white mb-8">
            The TEDxTulane Talk
          </h2>
          <div className="max-w-4xl">
            <CinemaEmbed
              source={{ type: 'youtube', id: 'Bq3Swc8q0CY' }}
              title="TEDxTulane — Youth Political Participation"
              subtitle="Organ Donation Advocacy"
              aspect="16:9"
            />
          </div>
        </motion.div>
      </div>
    </section>
  )
}
