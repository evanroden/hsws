'use client'

import { motion } from 'framer-motion'
import Image from 'next/image'
import Link from 'next/link'
import { useInView } from '@/lib/hooks'

export default function AboutTeaser() {
  const { ref, isInView } = useInView(0.2)

  return (
    <section className="section-padding bg-gradient-to-b from-slate-950 to-forest/10" ref={ref}>
      <div className="content-width">
        <div className="grid grid-cols-1 md:grid-cols-2 gap-12 md:gap-16 items-center">
          {/* Portrait */}
          <motion.div
            initial={{ opacity: 0, x: -30 }}
            animate={isInView ? { opacity: 1, x: 0 } : {}}
            transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
            className="aspect-square rounded-2xl bg-gradient-to-br from-white/5 to-white/[0.02] border border-white/5 relative overflow-hidden"
          >
            <Image
              src="/portrait.jpg"
              alt="Evan Roden"
              width={600}
              height={600}
              className="w-full h-full object-cover"
            />
          </motion.div>

          {/* Bio */}
          <motion.div
            initial={{ opacity: 0, x: 30 }}
            animate={isInView ? { opacity: 1, x: 0 } : {}}
            transition={{ duration: 0.7, delay: 0.2, ease: [0.16, 1, 0.3, 1] }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              About
            </span>
            <h2 className="font-serif text-heading text-white mt-3 mb-6">
              Engineer, advocate, filmmaker.
            </h2>
            <div className="space-y-4 text-titanium leading-relaxed">
              <p>
                Evan Roden co-founded The Youth Coalition For Organ Donation at seventeen
                and led it for seven years. He now manages central energy plants for
                Rochester Regional Health under ENFRA&apos;s $143.8 million
                Energy-as-a-Service partnership.
              </p>
              <p>
                He studied biomedical engineering at Tulane, worked in three research
                labs there, and gave a TEDxTulane talk on youth political participation.
                He has also shot and edited films as a cinematographer in New Orleans.
              </p>
            </div>
            <Link
              href="/about"
              className="inline-flex items-center gap-2 mt-8 text-sm text-white hover:text-copper transition-colors group"
            >
              <span>Full biography</span>
              <span className="group-hover:translate-x-1 transition-transform">→</span>
            </Link>
          </motion.div>
        </div>
      </div>
    </section>
  )
}
