'use client'

import { motion } from 'framer-motion'
import Link from 'next/link'
import { useInView } from '@/lib/hooks'
import TiltCard from '@/components/ui/TiltCard'

const pillars = [
  {
    title: 'Energy & Infrastructure',
    description:
      'From hospital central energy plants to ERP systems — designing, managing, and optimizing the critical infrastructure that keeps organizations running.',
    href: '/engineering-and-sustainability',
    stat: '$143.8M',
    statLabel: 'EaaS Partnership',
    gradient: 'from-forest/20 via-forest/5 to-transparent',
    borderColor: 'border-forest-light/20 hover:border-forest-light/40',
    accentColor: 'text-forest-light',
    icon: (
      <svg viewBox="0 0 48 48" className="w-10 h-10" fill="none" stroke="currentColor" strokeWidth="1.5">
        <path d="M24 4v40M4 24h40M12 12l24 24M36 12L12 36" opacity="0.3" />
        <circle cx="24" cy="24" r="8" className="text-forest-light" />
        <circle cx="24" cy="24" r="16" opacity="0.5" />
        <path d="M24 8v4M24 36v4M8 24h4M36 24h4" strokeWidth="2" />
      </svg>
    ),
  },
  {
    title: 'Civic Advocacy',
    description:
      'Seven years leading organ donation reform. Climate policy fellowship. Award-winning urban revitalization. Building systems that serve everyone.',
    href: '/advocacy-and-civic',
    stat: '100K+',
    statLabel: 'On the waiting list',
    gradient: 'from-copper/15 via-copper/5 to-transparent',
    borderColor: 'border-copper/20 hover:border-copper/40',
    accentColor: 'text-copper',
    icon: (
      <svg viewBox="0 0 48 48" className="w-10 h-10" fill="none" stroke="currentColor" strokeWidth="1.5">
        <path d="M24 44s-16-8.4-16-20.2C8 14.6 15.2 8 24 8s16 6.6 16 15.8C40 35.6 24 44 24 44z" className="text-copper" />
        <path d="M24 20v8M20 24h8" strokeWidth="2" />
      </svg>
    ),
  },
  {
    title: 'Visual Arts',
    description:
      'Cinematography with Claiborne Avenue Productions. Digital marketing for Tulane. Kiln-formed glass art. Vogue Italy editorial modeling.',
    href: '/studio',
    stat: '4',
    statLabel: 'Creative Disciplines',
    gradient: 'from-titanium/10 via-titanium/5 to-transparent',
    borderColor: 'border-titanium/20 hover:border-titanium/40',
    accentColor: 'text-titanium',
    icon: (
      <svg viewBox="0 0 48 48" className="w-10 h-10" fill="none" stroke="currentColor" strokeWidth="1.5">
        <rect x="6" y="14" width="36" height="22" rx="3" />
        <circle cx="24" cy="25" r="6" className="text-titanium" />
        <circle cx="24" cy="25" r="2" />
        <path d="M10 14V10h6M32 14V10h6" strokeWidth="2" />
      </svg>
    ),
  },
]

export default function BentoGrid() {
  const { ref, isInView } = useInView(0.1)

  return (
    <section className="section-padding bg-slate-950 relative" ref={ref}>
      <div className="content-width">
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="mb-12"
        >
          <div className="flex items-center gap-3 mb-3">
            <span className="w-8 h-px bg-copper/50" />
            <span className="font-mono text-xs tracking-[0.2em] uppercase text-copper">
              Three Pillars
            </span>
          </div>
          <h2 className="font-serif text-heading text-white">
            Where disciplines converge.
          </h2>
        </motion.div>

        <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
          {pillars.map((pillar, i) => (
            <motion.div
              key={pillar.title}
              initial={{ opacity: 0, y: 40 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.7, delay: i * 0.15, ease: [0.16, 1, 0.3, 1] }}
            >
              <TiltCard maxTilt={6} glare={0.1}>
                <Link href={pillar.href} className="group block h-full" data-cursor="Explore">
                  <div
                    className={`relative rounded-2xl border ${pillar.borderColor} bg-gradient-to-br ${pillar.gradient} p-8 h-full min-h-[320px] flex flex-col justify-between overflow-hidden transition-all duration-500`}
                  >
                    <div className="absolute top-0 right-0 w-48 h-48 bg-white/[0.015] rounded-full -translate-y-1/2 translate-x-1/2 blur-3xl transition-all duration-700 group-hover:w-64 group-hover:h-64 group-hover:bg-white/[0.03]" />

                    <div>
                      <div className={`${pillar.accentColor} mb-6 transition-transform duration-500 group-hover:scale-110 group-hover:translate-x-1`}>
                        {pillar.icon}
                      </div>
                      <h3 className="font-serif text-xl md:text-2xl text-white mb-3 group-hover:text-copper transition-colors duration-300">
                        {pillar.title}
                      </h3>
                      <p className="text-titanium text-sm leading-relaxed">
                        {pillar.description}
                      </p>
                    </div>

                    <div className="mt-8 pt-6 border-t border-white/5">
                      <span className="font-serif text-2xl text-white">{pillar.stat}</span>
                      <span className="block font-mono text-xs text-titanium/60 mt-1">
                        {pillar.statLabel}
                      </span>
                    </div>

                    <div className="absolute bottom-8 right-8 opacity-0 translate-x-2 group-hover:opacity-100 group-hover:translate-x-0 transition-all duration-300">
                      <span className="text-copper text-lg">&rarr;</span>
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
