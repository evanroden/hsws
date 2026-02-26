'use client'

import { motion } from 'framer-motion'
import Link from 'next/link'
import { useInView } from '@/lib/hooks'

const featured = [
  {
    title: 'ENFRA × Rochester Regional Health',
    category: 'Energy-as-a-Service',
    description:
      '$143.8 million, 30-year EaaS partnership delivering $354.6M in guaranteed savings and 52.5% reduction in purchased electricity.',
    href: '/engineering-and-sustainability/enfra',
    tag: 'Engineering',
    color: 'bg-forest/20 text-forest-light',
  },
  {
    title: 'The YCOD — Opt-Out Organ Donation',
    category: 'Legislative Advocacy',
    description:
      'Founded at age 17. Seven years of advocacy to change New York\'s organ donor designation system, addressing the state\'s lowest-in-nation registration rate.',
    href: '/advocacy-and-civic/ycod',
    tag: 'Advocacy',
    color: 'bg-copper/20 text-copper',
  },
  {
    title: 'VA Prosthetics — 3D-Printed Devices',
    category: 'Biomedical Engineering',
    description:
      'Designed and modeled custom prosthetic devices in Fusion 360 for 3D printing, restoring autonomy to American veterans in the Southern Louisiana region.',
    href: '/engineering-and-sustainability/research/va-prosthetics',
    tag: 'Research',
    color: 'bg-clinical/20 text-titanium',
  },
  {
    title: 'Claiborne Avenue Productions',
    category: 'Cinematography',
    description:
      'Camera operator and editor under Albert J. Moten, Jr., working with BlackMagic 6K and Sony a7s II on productions in New Orleans.',
    href: '/studio/cinematography',
    tag: 'Creative',
    color: 'bg-titanium/20 text-titanium',
  },
]

export default function FeaturedWork() {
  const { ref, isInView } = useInView(0.1)

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
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Featured Work
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
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
              <Link href={item.href} className="group block">
                <div className="glass rounded-xl overflow-hidden transition-all duration-500 group-hover:bg-white/10 group-hover:border-white/20 group-hover:scale-[1.01]">
                  {/* Placeholder image area */}
                  <div className="aspect-[16/9] bg-gradient-to-br from-white/5 to-white/[0.02] relative overflow-hidden">
                    <div className="absolute inset-0 flex items-center justify-center">
                      <span className="font-mono text-xs text-titanium/30 tracking-widest uppercase">
                        {item.category}
                      </span>
                    </div>
                    {/* Subtle grid pattern */}
                    <div
                      className="absolute inset-0 opacity-[0.03]"
                      style={{
                        backgroundImage:
                          'radial-gradient(circle at 1px 1px, white 1px, transparent 1px)',
                        backgroundSize: '20px 20px',
                      }}
                    />
                  </div>

                  <div className="p-6 md:p-8">
                    <div className="flex items-center gap-3 mb-4">
                      <span
                        className={`inline-block px-2.5 py-1 rounded-full text-xs font-mono ${item.color}`}
                      >
                        {item.tag}
                      </span>
                      <span className="text-titanium/50 text-xs font-mono">
                        {item.category}
                      </span>
                    </div>
                    <h3 className="font-serif text-xl text-white mb-2 group-hover:text-copper transition-colors">
                      {item.title}
                    </h3>
                    <p className="text-titanium text-sm leading-relaxed">
                      {item.description}
                    </p>
                    <div className="mt-4 flex items-center gap-2 text-sm text-titanium group-hover:text-white transition-colors">
                      <span>Read case study</span>
                      <span className="group-hover:translate-x-1 transition-transform">→</span>
                    </div>
                  </div>
                </div>
              </Link>
            </motion.div>
          ))}
        </div>
      </div>
    </section>
  )
}
