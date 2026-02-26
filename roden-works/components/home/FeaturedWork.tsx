'use client'

import { motion, useInView } from 'framer-motion'
import Link from 'next/link'
import { useRef } from 'react'
import TiltCard from '@/components/ui/TiltCard'
import { ProjectIllustration } from '@/components/ui/ProjectIllustrations'

const featured = [
  {
    title: 'ENFRA × Rochester Regional Health',
    category: 'Energy-as-a-Service',
    slug: 'enfra',
    description:
      '$143.8 million, 30-year EaaS partnership delivering $354.6M in guaranteed savings and 52.5% reduction in purchased electricity.',
    href: '/engineering-and-sustainability/enfra',
    tag: 'Engineering',
    color: 'bg-forest/20 text-forest-light border-forest-light/20',
    gradientFrom: 'from-forest/10',
  },
  {
    title: 'The YCOD — Opt-Out Organ Donation',
    category: 'Legislative Advocacy',
    slug: 'ycod',
    description:
      'Founded at age 17. Seven years of advocacy to change New York\'s organ donor designation system, addressing the state\'s lowest-in-nation registration rate.',
    href: '/advocacy-and-civic/ycod',
    tag: 'Advocacy',
    color: 'bg-copper/20 text-copper border-copper/20',
    gradientFrom: 'from-copper/10',
  },
  {
    title: 'VA Prosthetics — 3D-Printed Devices',
    category: 'Biomedical Engineering',
    slug: 'va-prosthetics',
    description:
      'Designed and modeled custom prosthetic devices in Fusion 360 for 3D printing, restoring autonomy to American veterans in the Southern Louisiana region.',
    href: '/engineering-and-sustainability/research/va-prosthetics',
    tag: 'Research',
    color: 'bg-clinical/20 text-titanium border-titanium/20',
    gradientFrom: 'from-titanium/5',
  },
  {
    title: 'Claiborne Avenue Productions',
    category: 'Cinematography',
    slug: 'cinematography',
    description:
      'Camera operator and editor under Albert J. Moten, Jr., working with BlackMagic 6K and Sony a7s II on productions in New Orleans.',
    href: '/studio/cinematography',
    tag: 'Creative',
    color: 'bg-titanium/20 text-titanium border-titanium/20',
    gradientFrom: 'from-copper/5',
  },
]

export default function FeaturedWork() {
  const ref = useRef<HTMLElement>(null)
  const isInView = useInView(ref, { once: true, margin: '-10%' })

  return (
    <section className="section-padding bg-slate-950" ref={ref}>
      <div className="content-width">
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="flex items-end justify-between mb-12"
        >
          <div>
            <div className="flex items-center gap-3 mb-3">
              <span className="w-8 h-px bg-copper/50" />
              <span className="font-mono text-xs tracking-[0.2em] uppercase text-copper">
                Featured Work
              </span>
            </div>
            <h2 className="font-serif text-heading text-white">
              Selected projects.
            </h2>
          </div>
        </motion.div>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          {featured.map((item, i) => (
            <motion.div
              key={item.title}
              initial={{ opacity: 0, y: 30 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{
                duration: 0.6,
                delay: i * 0.1,
                ease: [0.16, 1, 0.3, 1],
              }}
            >
              <TiltCard maxTilt={4} glare={0.08}>
                <Link href={item.href} className="group block" data-cursor="View">
                  <div className="glass rounded-xl overflow-hidden transition-all duration-500 group-hover:bg-white/10 group-hover:border-white/20">
                    {/* Illustrated image area */}
                    <div className={`aspect-[16/9] bg-gradient-to-br ${item.gradientFrom} to-white/[0.02] relative overflow-hidden`}>
                      <div className="absolute inset-0">
                        <ProjectIllustration slug={item.slug} variant="featured" />
                      </div>
                      {/* Hover reveal line */}
                      <motion.div
                        className="absolute bottom-0 left-0 right-0 h-px bg-gradient-to-r from-copper/60 via-copper/20 to-transparent"
                        initial={{ scaleX: 0 }}
                        whileInView={{ scaleX: 1 }}
                        transition={{ duration: 1, delay: 0.3 + i * 0.1 }}
                        style={{ originX: 0 }}
                      />
                    </div>

                    <div className="p-6 md:p-8">
                      <div className="flex items-center gap-3 mb-4">
                        <span
                          className={`inline-block px-2.5 py-1 rounded-full text-xs font-mono border ${item.color}`}
                        >
                          {item.tag}
                        </span>
                        <span className="text-titanium/40 text-xs font-mono">
                          {item.category}
                        </span>
                      </div>
                      <h3 className="font-serif text-xl text-white mb-2 group-hover:text-copper transition-colors duration-300">
                        {item.title}
                      </h3>
                      <p className="text-titanium text-sm leading-relaxed">
                        {item.description}
                      </p>
                      <div className="mt-4 flex items-center gap-2 text-sm text-titanium/60 group-hover:text-copper transition-colors duration-300">
                        <span>Read case study</span>
                        <span className="group-hover:translate-x-1.5 transition-transform duration-300">&rarr;</span>
                      </div>
                    </div>
                  </div>
                </Link>
              </TiltCard>
            </motion.div>
          ))}
        </div>
      </div>
    </section>
  )
}
