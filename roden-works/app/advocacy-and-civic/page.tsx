import type { Metadata } from 'next'
import PageHero from '@/components/ui/PageHero'
import AnimatedCard from '@/components/ui/AnimatedCard'
import { ProjectIllustration } from '@/components/ui/ProjectIllustrations'

export const metadata: Metadata = {
  title: 'Advocacy & Civic Impact — Organ Donation, Climate & Urban Policy',
  description:
    'Organ donation law, climate policy, rural broadband, and urban planning.',
}

const projects = [
  {
    href: '/advocacy-and-civic/ycod',
    slug: 'ycod',
    title: 'The Youth Coalition For Organ Donation',
    description: 'Co-founded at 17. Seven years of advocacy for presumed consent organ donation legislation in New York.',
    label: 'Founded 2017',
  },
  {
    href: '/advocacy-and-civic/our-climate',
    slug: 'our-climate',
    title: 'Our Climate Fellowship',
    description: 'Youth-led climate policy advocacy at the state and federal level, including campaign organizing and meetings with representatives.',
    label: 'Fellowship',
  },
  {
    href: '/advocacy-and-civic/tabi',
    slug: 'tabi',
    title: 'Aurora Broadband Initiative',
    description: 'Broadband access proposal for underserved rural households in Western New York.',
    label: 'Digital Equity',
  },
  {
    href: '/advocacy-and-civic/nola-east',
    slug: 'nola-east',
    title: 'New Orleans East Revitalization',
    description: 'Revitalization plan covering solar energy, transit, green housing, and disaster planning. Won the C40 Reinventing Cities Award.',
    label: 'C40 Award',
  },
  {
    href: '/advocacy-and-civic/midtown-metairie',
    slug: 'midtown-metairie',
    title: 'Midtown Metairie',
    description: 'Urban planning proposal for Louisiana\'s most populous unincorporated community.',
    label: 'Urban Planning',
  },
  {
    href: '/advocacy-and-civic/partnership',
    slug: 'partnership',
    title: 'Partnership for Public Service',
    description: 'Federal workforce project with SAMHSA. Employee engagement scores rose from about 37 to 74 over the broader collaboration.',
    label: 'Federal Service',
  },
]

export default function AdvocacyPage() {
  return (
    <>
      <PageHero
        title="Advocacy & Civic Impact"
        subtitle="Organ donation law, climate policy, rural broadband, and urban planning."
        label="Public Service"
        variant="forest"
      />
      <section className="section-padding bg-slate-950">
        <div className="content-width">
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            {projects.map((project, i) => (
              <AnimatedCard key={project.href} index={i} {...project}>
                <div className="mb-4 -mx-2 opacity-80">
                  <ProjectIllustration slug={project.slug} variant="card" />
                </div>
              </AnimatedCard>
            ))}
          </div>
        </div>
      </section>
    </>
  )
}
