import type { Metadata } from 'next'
import PageHero from '@/components/ui/PageHero'
import AnimatedCard from '@/components/ui/AnimatedCard'

export const metadata: Metadata = {
  title: 'Advocacy & Civic Impact',
  description:
    'From organ donation policy to broadband access — building systems that serve everyone.',
}

const projects = [
  {
    href: '/advocacy-and-civic/ycod',
    title: 'The Youth Coalition For Organ Donation',
    description: 'Founded at age 17. Seven years of advocacy to reform New York\'s organ donor designation system through presumed consent legislation.',
    label: 'Founded 2017',
  },
  {
    href: '/advocacy-and-civic/our-climate',
    title: 'Our Climate Fellowship',
    description: 'Youth-led climate policy advocacy at state and federal level. Campaign organizing, representative meetings, and community outreach.',
    label: 'Fellowship',
  },
  {
    href: '/advocacy-and-civic/tabi',
    title: 'Aurora Broadband Initiative',
    description: 'Broadband access proposal for rural Western New York, addressing the digital divide in Cayuga County.',
    label: 'Digital Equity',
  },
  {
    href: '/advocacy-and-civic/nola-east',
    title: 'New Orleans East Revitalization',
    description: 'C40 Reinventing Cities Award winner. Urban revitalization plan covering solar energy, transit, green housing, and disaster planning.',
    label: 'C40 Award',
  },
  {
    href: '/advocacy-and-civic/midtown-metairie',
    title: 'Midtown Metairie',
    description: 'Urban planning proposal for Louisiana\'s most populous unincorporated community.',
    label: 'Urban Planning',
  },
  {
    href: '/advocacy-and-civic/partnership',
    title: 'Partnership for Public Service',
    description: 'Federal workforce team supporting SAMHSA improvement. Engagement scores doubled from 37 to 74 during collaboration.',
    label: 'Federal Service',
  },
]

export default function AdvocacyPage() {
  return (
    <>
      <PageHero
        title="Advocacy & Civic Impact"
        subtitle="From organ donation policy to broadband access — building systems that serve everyone."
        label="Public Service"
        variant="forest"
      />
      <section className="section-padding bg-slate-950">
        <div className="content-width">
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            {projects.map((project, i) => (
              <AnimatedCard key={project.href} index={i} {...project} />
            ))}
          </div>
        </div>
      </section>
    </>
  )
}
