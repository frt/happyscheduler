import { DebugElement } from '@angular/core';
import { TestBed, async, inject } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { HttpModule, Http } from '@angular/http';
import { InfoMessagesService } from '../info-messages.service';

import { Observable} from 'rxjs/Observable';
import 'rxjs/add/observable/of';

import { ComponentFixtureAutoDetect } from '@angular/core/testing';
import { By } from '@angular/platform-browser';

import { TaskListComponent } from './task-list.component';

describe('TaskListComponent', () => {
    let fixture;
    let component: TaskListComponent;
    let el;

    beforeEach(async(() => {
        TestBed.configureTestingModule({
            declarations: [
                TaskListComponent
            ],
            imports: [
                RouterTestingModule,
                HttpModule
            ],
            providers: [
                { provide: ComponentFixtureAutoDetect, useValue: true }
            ]
        }).compileComponents();
        fixture = TestBed.createComponent(TaskListComponent);
        el = fixture.debugElement.nativeElement;

        component = fixture.componentInstance;
        // tasks array example:
        component.tasks = [{
            time: 30,
            done: false,
            name: 'test remove',
            id: 1,
            happy: true
        }];
    }));

    it('should not have an <hr> element',  async(() => {
        expect(el.querySelectorAll('hr').length).toEqual(0);
    }));

    it('should have a delete button when there is more than 0 tasks', async(() => {
        expect(el.querySelectorAll('button.glyphicon-remove').length)
            .toBeGreaterThan(0);
    }));

    it('should have a done button when there is more than 0 tasks', async(() => {
        expect(el.querySelectorAll('button.glyphicon-ok').length)
            .toBeGreaterThan(0);
    }));

    it('should delete a task when delete button is clicked', async(() => {
        const http = fixture.debugElement.injector.get(Http);
        spyOn(http, 'delete').and.returnValue(Observable.of('DELETED'));

        expect(el.querySelectorAll('.task-item').length).toEqual(1);

        fixture.debugElement.query(By.css('button.glyphicon-remove')).triggerEventHandler('click', null);
        fixture.detectChanges();

        expect(http.delete).toHaveBeenCalledWith('../tasks/1');
        expect(el.querySelectorAll('.task-item').length).toEqual(0);
    }));
});
