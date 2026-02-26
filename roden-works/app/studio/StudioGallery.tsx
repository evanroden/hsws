'use client'

import { motion, AnimatePresence } from 'framer-motion'
import { useState } from 'react'
import Link from 'next/link'
import { useInView } from '@/lib/hooks'
import { ProjectIllustration } from '@/components/ui/ProjectIllustrations'

type Category = 'all' | 'cinematography' | 'photography' | 'glass-art' | 'modeling'

const categories: { key: Category; label: string }[] = [
  { key: 'all', label: 'All Work' },
  { key: 'cinematography', label: 'Cinematography' },
  { key: 'photography', label: 'Photography' },
  { key: 'glass-art', label: 'Glass Art' },
  { key: 'modeling', label: 'Modeling' },
]

const items = [
  {
    title: 'Claiborne Avenue Productions',
    category: 'cinematography' as const,
    slug: 'claiborne-avenue',
    description: 'Camera operator and editor under Albert J. Moten, Jr. BlackMagic 6K, Sony a7s II.',
    href: '/studio/cinematography',
    aspect: 'aspect-video',
  },
  {
    title: 'Tulane Freeman School',
    category: 'cinematography' as const,
    slug: 'tulane-freeman',
    description: 'Digital marketing content. Short-form videos, interviews, event coverage.',
    href: '/studio/cinematography',
    aspect: 'aspect-video',
  },
  {
    title: 'Fractured Futures',
    category: 'glass-art' as const,
    slug: 'fractured-futures',
    description: 'Kiln forming and glass fusing fine art. Exploring fractured forms and translucent light.',
    href: '/studio/glass-art',
    aspect: 'aspect-square',
  },
  {
    title: 'Vogue Italy — BizarrAudi',
    category: 'modeling' as const,
    slug: 'vogue-italy',
    description: '2020 runway modeling for Vogue Italy\'s SchoolTime collection.',
    href: '/studio/modeling',
    aspect: 'aspect-[3/4]',
  },
  {
    title: 'Documentary Work',
    category: 'cinematography' as const,
    slug: 'documentary-work',
    description: 'Plato\'s Cave, The Bridge — narrative and documentary filmmaking.',
    href: '/studio/cinematography',
    aspect: 'aspect-video',
  },
  {
    title: 'Medium Format Photography',
    category: 'photography' as const,
    slug: 'medium-format-photography',
    description: 'Medium-format and full-frame photography portfolio.',
    href: '/studio/photography',
    aspect: 'aspect-[4/5]',
  },
  {
    title: 'Aurora Theatre',
    category: 'cinematography' as const,
    slug: 'aurora-theatre',
    description: 'Production and promotional content for Aurora Theatre.',
    href: '/studio/cinematography',
    aspect: 'aspect-video',
  },
  {
    title: 'Buffalo Central Terminal',
    category: 'cinematography' as const,
    slug: 'buffalo-central-terminal',
    description: 'Architectural and cultural documentation.',
    href: '/studio/cinematography',
    aspect: 'aspect-video',
  },
]

export default function StudioGallery() {
  const [activeFilter, setActiveFilter] = useState<Category>('all')
  const { ref, isInView } = useInView(0.05)

  const filtered = activeFilter === 'all' ? items : items.filter((i) => i.category === activeFilter)

  return (
    <section className="section-padding bg-slate-950" ref={ref}>
      <div className="content-width">
        {/* Filter bar */}
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="flex flex-wrap gap-3 mb-12"
        >
          {categories.map((cat) => (
            <button
              key={cat.key}
              onClick={() => setActiveFilter(cat.key)}
              className={`px-4 py-2 rounded-lg text-sm font-mono transition-all duration-300 ${
                activeFilter === cat.key
                  ? 'bg-copper text-white'
                  : 'bg-white/5 text-titanium hover:bg-white/10'
              }`}
            >
              {cat.label}
            </button>
          ))}
        </motion.div>

        {/* Masonry grid */}
        <motion.div layout className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
          <AnimatePresence mode="popLayout">
            {filtered.map((item, i) => (
              <motion.div
                key={item.title}
                layout
                initial={{ opacity: 0, scale: 0.9 }}
                animate={{ opacity: 1, scale: 1 }}
                exit={{ opacity: 0, scale: 0.9 }}
                transition={{ duration: 0.4, delay: i * 0.05 }}
              >
                <Link href={item.href} className="group block">
                  <div className="glass rounded-xl overflow-hidden transition-all duration-500 group-hover:bg-white/10 group-hover:border-white/20 group-hover:scale-[1.02]">
                    {/* Illustrated thumbnail */}
                    <div className={`${item.aspect} bg-gradient-to-br from-white/5 to-white/[0.02] relative overflow-hidden`}>
                      <div className="absolute inset-0">
                        <ProjectIllustration slug={item.slug} variant="studio" />
                      </div>
                    </div>
                    <div className="p-5">
                      <span className="font-mono text-xs text-copper capitalize">{item.category.replace('-', ' ')}</span>
                      <h3 className="font-serif text-lg text-white mt-1 group-hover:text-copper transition-colors">
                        {item.title}
                      </h3>
                      <p className="text-titanium text-sm mt-2 leading-relaxed">{item.description}</p>
                    </div>
                  </div>
                </Link>
              </motion.div>
            ))}
          </AnimatePresence>
        </motion.div>
      </div>
    </section>
  )
}
