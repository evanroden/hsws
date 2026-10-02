'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'
import { Cite } from '@/components/ui/Sources'
import { ABOUT_SOURCES as S } from './sources'

// "Best Debater & Speaker" and "Best Student of 2022" were removed: no named issuer or event
// (org fields were placeholders). Re-add with the real issuer if they're confirmed.
const awards: { title: string; org: string; description: string; sources?: string[] }[] = [
  {
    // https://mynews13.com/fl/orlando/news/2021/09/24/wny-teens-nominated-for-american-red-cross-award-for-organ-donation-coalition
    title: 'Real Heroes Education Award Nominee, 2021',
    org: 'American Red Cross',
    sources: ['red-cross-nomination'],
    description: 'Nominated with three fellow co-founders for starting The YCOD, an organ donation advocacy organization.',
  },
  {
    // Honorable mention (team "People First", six Loyola students and one Tulane student), not the C40 winner.
    // The New Orleans winner was Imperial College London's "ReNew Orleans".
    // https://css.loyno.edu/news/sep-14-2023_loyola-team-wins-honorable-mention-global-students-reinventing-cities-competition
    // https://www.c40reinventingcities.org/en/events/new-orleans-winning-team-present-their-project-to-mayor-latoya-cantrell-1828.html
    title: 'Students Reinventing Cities, Honorable Mention',
    org: 'C40 Cities · 2023',
    sources: ['loyola-c40'],
    description: 'Team plan for New Orleans East covering disaster planning, energy, transit, and housing. Mayor LaToya Cantrell honored the team and asked the team to present it.',
  },
  {
    title: 'Boy of the Year',
    org: 'Boys & Girls Club',
    description: 'Youth recognition from the Boys & Girls Club.',
  },
  {
    title: 'National Latin Honor Society Honoree',
    org: 'National Latin Honor Society',
    description: 'Honored for achievement in Latin.',
  },
]

export default function Awards() {
  const { ref, isInView } = useInView(0.1)

  return (
    <section className="section-padding bg-slate-950" ref={ref}>
      <div className="content-width">
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="mb-12"
        >
          <span className="font-mono text-xs tracking-widest uppercase text-copper">
            Awards & Honors
          </span>
          <h2 className="font-serif text-heading text-white mt-3">Recognition.</h2>
        </motion.div>

        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
          {awards.map((award, i) => (
            <motion.div
              key={award.title}
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.5, delay: i * 0.08 }}
              className="glass rounded-xl p-6 group hover:bg-white/10 hover:border-copper/20 transition-all duration-300"
            >
              <div className="w-8 h-8 rounded-full bg-copper/10 flex items-center justify-center mb-4">
                <svg viewBox="0 0 24 24" className="w-4 h-4 text-copper" fill="currentColor">
                  <path d="M12 2l3.09 6.26L22 9.27l-5 4.87 1.18 6.88L12 17.77l-6.18 3.25L7 14.14 2 9.27l6.91-1.01L12 2z" />
                </svg>
              </div>
              <h3 className="font-serif text-lg text-white mb-1">
                {award.title}
                {award.sources && <Cite sources={S} id={award.sources} />}
              </h3>
              <p className="text-copper text-xs font-mono mb-2">{award.org}</p>
              <p className="text-titanium text-sm leading-relaxed">{award.description}</p>
            </motion.div>
          ))}
        </div>
      </div>
    </section>
  )
}
